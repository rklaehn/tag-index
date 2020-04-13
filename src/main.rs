use maplit::btreeset;
use reduce::Reduce;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::{
    collections::{BTreeMap, BTreeSet},
    ops::{BitAnd, BitOr},
};

/// a compact index
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Index {
    /// the strings table
    strings: BTreeSet<String>,
    /// indices in these sets are guaranteed to correspond to strings in the strings table
    elements: Vec<BTreeSet<u32>>,
}

impl Serialize for Index {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        // serialize as a tuple so it is guaranteed that the strings table is before the indices,
        // in case we ever want to write a clever visitor that matches without building an AST
        // of the deserialized result.
        (&self.strings, &self.elements).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Index {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> std::result::Result<Self, D::Error> {
        let (strings, elements) = <(Vec<String>, Vec<BTreeSet<u32>>)>::deserialize(deserializer)?;
        // ensure valid indices
        for s in elements.iter() {
            for x in s {
                if strings.get(*x as usize).is_none() {
                    return Err(serde::de::Error::custom("invalid string index"));
                }
            }
        }
        Ok(Index {
            strings: strings.into_iter().collect(),
            elements,
        })
    }
}

impl Index {
    /// given a query expression in Dnf form, returns all matching indices
    pub fn matching(&self, query: Dnf) -> Vec<usize> {
        // lookup all strings and translate them into indices.
        // if a single index does not match, the query can not match at all.
        fn lookup(s: &BTreeSet<String>, t: &BTreeMap<&str, u32>) -> Option<BTreeSet<u32>> {
            s.iter()
                .map(|x| t.get(&x.as_ref()).cloned())
                .collect::<Option<_>>()
        }
        // mapping from strings to indices
        let strings = self
            .strings
            .iter()
            .enumerate()
            .map(|(i, s)| (s.as_ref(), i as u32))
            .collect::<BTreeMap<&str, u32>>();
        // translate the query from strings to indices
        let query = query
            .0
            .iter()
            .filter_map(|s| lookup(s, &strings))
            .collect::<Vec<_>>();
        // not a single query can possibly match, no need to iterate.
        if query.is_empty() {
            return Vec::new();
        }
        // check the remaining queries
        self.elements
            .iter()
            .enumerate()
            .filter_map(|(i, e)| {
                if query.iter().any(|x| x.is_subset(e)) {
                    Some(i)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn as_elements<'a>(&'a self) -> Vec<BTreeSet<&'a str>> {
        let strings = self.strings.iter().map(|x| x.as_ref()).collect::<Vec<_>>();
        self
            .elements
            .iter()
            .map(|is| {
                is.iter()
                    .map(|i| strings[*i as usize])
                    .collect::<BTreeSet<_>>()
            })
            .collect()
    }

    pub fn from_elements(e: &[BTreeSet<&str>]) -> Index {
        let mut strings = BTreeSet::new();
        for a in e.iter() {
            strings.extend(a.iter().cloned());
        }
        let indices = strings
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, e)| (e, i as u32))
            .collect::<BTreeMap<_, _>>();
        let elements = e
            .iter()
            .map(|a| a.iter().map(|e| indices[e]).collect::<BTreeSet<u32>>())
            .collect::<Vec<_>>();
        let strings = strings.into_iter().map(|x| x.to_owned()).collect();
        Index { strings, elements }
    }
}

/// a boolean expression, consisting of literals, union and intersection.
///
/// no attempt of simplification is made, except flattening identical operators.
///
/// `And([And([a,b]),c])` will be flattened to `And([a,b,c])`.
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Expression {
    Literal(String),
    And(Vec<Expression>),
    Or(Vec<Expression>),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        fn child_to_string(x: &Expression) -> String {
            if let Expression::Or(_) = x {
                format!("({})", x)
            } else {
                x.to_string()
            }
        }
        write!(
            f,
            "{}",
            match self {
                Expression::Literal(text) => text.clone(),
                Expression::And(es) => es.iter().map(child_to_string).collect::<Vec<_>>().join("&"),
                Expression::Or(es) => es.iter().map(child_to_string).collect::<Vec<_>>().join("|"),
            }
        )
    }
}

/// Disjunctive normal form of a boolean query expression
///
/// https://en.wikipedia.org/wiki/Disjunctive_normal_form
///
/// This is an unique represenation of a query using literals, union and intersection.
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct Dnf(BTreeSet<BTreeSet<String>>);

impl Dnf {
    fn literal(text: String) -> Self {
        Self(btreeset![btreeset![text]])
    }

    /// converts the disjunctive normal form back to an expression
    pub fn expression(self) -> Expression {
        self.0
            .into_iter()
            .map(Dnf::and_expr)
            .reduce(Expression::bitor)
            .unwrap()
    }

    fn and_expr(v: BTreeSet<String>) -> Expression {
        v.into_iter()
            .map(Expression::literal)
            .reduce(Expression::bitand)
            .unwrap()
    }
}

impl Expression {
    pub fn literal(text: String) -> Self {
        Self::Literal(text)
    }

    fn or(e: Vec<Expression>) -> Self {
        Self::Or(
            e.into_iter()
                .flat_map(|c| match c {
                    Self::Or(es) => es,
                    x => vec![x],
                })
                .collect(),
        )
    }

    fn and(e: Vec<Expression>) -> Self {
        Self::And(
            e.into_iter()
                .flat_map(|c| match c {
                    Self::And(es) => es,
                    x => vec![x],
                })
                .collect(),
        )
    }

    pub fn dnf(self) -> Dnf {
        match self {
            Expression::Literal(x) => Dnf::literal(x),
            Expression::Or(es) => es.into_iter().map(|x| x.dnf()).reduce(Dnf::bitor).unwrap(),
            Expression::And(es) => es.into_iter().map(|x| x.dnf()).reduce(Dnf::bitand).unwrap(),
        }
    }
}

impl BitOr for Expression {
    type Output = Expression;
    fn bitor(self, that: Self) -> Self {
        Expression::or(vec![self, that])
    }
}

impl BitAnd for Expression {
    type Output = Expression;
    fn bitand(self, that: Self) -> Self {
        Expression::and(vec![self, that])
    }
}

fn insert_unless_redundant(aa: &mut BTreeSet<BTreeSet<String>>, b: BTreeSet<String>) {
    let mut to_remove = None;
    for a in aa.iter() {
        if a.is_subset(&b) {
            // a is larger than b. E.g. x | x&y
            // keep a, b is redundant
            return;
        } else if a.is_superset(&b) {
            // a is smaller than b, E.g. x&y | x
            // remove a, keep b
            to_remove = Some(a.clone());
        }
    }
    if let Some(r) = to_remove {
        aa.remove(&r);
    }
    aa.insert(b);
}

impl From<Expression> for Dnf {
    fn from(value: Expression) -> Self {
        value.dnf()
    }
}

impl From<Dnf> for Expression {
    fn from(value: Dnf) -> Self {
        value.expression()
    }
}

impl BitAnd for Dnf {
    type Output = Dnf;
    fn bitand(self, that: Self) -> Self {
        let mut rs = BTreeSet::new();
        for a in self.0.iter() {
            for b in that.0.iter() {
                let mut r = BTreeSet::new();
                r.extend(a.iter().cloned());
                r.extend(b.iter().cloned());
                insert_unless_redundant(&mut rs, r);
            }
        }
        Dnf(rs)
    }
}

impl BitOr for Dnf {
    type Output = Dnf;
    fn bitor(self, that: Self) -> Self {
        let mut rs = self.0;
        for b in that.0 {
            insert_unless_redundant(&mut rs, b);
        }
        Dnf(rs)
    }
}

fn l(x: &str) -> Expression {
    Expression::literal(x.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::{quickcheck, Arbitrary, Gen};
    use rand::seq::SliceRandom;

    #[test]
    fn test_dnf_intersection_1() {
        let a = l("a");
        let b = l("b");
        let c = l("c");
        let expr = c & (a | b);
        let c = expr.dnf().expression().to_string();
        assert_eq!(c, "a&c|b&c");
    }

    #[test]
    fn test_dnf_intersection_2() {
        let a = l("a");
        let b = l("b");
        let c = l("c");
        let d = l("d");
        let expr = (d | c) & (b | a);
        let c = expr.dnf().expression().to_string();
        assert_eq!(c, "a&c|a&d|b&c|b&d");
    }

    #[test]
    fn test_dnf_simplify_1() {
        let a = l("a");
        let b = l("b");
        let expr = (a.clone() | b) & a;
        let c = expr.dnf().expression().to_string();
        assert_eq!(c, "a");
    }

    #[test]
    fn test_dnf_simplify_2() {
        let a = l("a");
        let b = l("b");
        let expr = (a.clone() & b) | a;
        let c = expr.dnf().expression().to_string();
        assert_eq!(c, "a");
    }

    #[test]
    fn test_dnf_simplify_3() {
        let a = l("a");
        let b = l("b");
        let expr = (a.clone() | b) | a;
        let c = expr.dnf().expression().to_string();
        assert_eq!(c, "a|b");
    }

    #[test]
    fn test_matching_1() {
        let index = Index::from_elements(&vec![
            btreeset! {"a"},
            btreeset! {"a", "b"},
            btreeset! {"a"},
            btreeset! {"a", "b"},
        ]);
        let expr = l("a") | l("b");
        assert_eq!(index.matching(expr.dnf()), vec![0,1,2,3]);
        let expr = l("a") & l("b");
        assert_eq!(index.matching(expr.dnf()), vec![1,3]);
        let expr = l("c") & l("d");
        assert!(index.matching(expr.dnf()).is_empty());
    }

    #[test]
    fn test_matching_2() {
        let index = Index::from_elements(&vec![
            btreeset! {"a", "b"},
            btreeset! {"b", "c"},
            btreeset! {"c", "a"},
            btreeset! {"a", "b"},
        ]);
        let expr = l("a") | l("b") | l("c");
        assert_eq!(index.matching(expr.dnf()), vec![0,1,2,3]);
        let expr = l("a") & l("b");
        assert_eq!(index.matching(expr.dnf()), vec![0,3]);
        let expr = l("a") & l("b") & l("c");
        assert!(index.matching(expr.dnf()).is_empty());
    }

    #[test]
    fn test_deser_error() {
        // negative index - serde should catch this
        let e1 = r#"[["a","b"],[[0],[0,1],[0],[0,-1]]]"#;
        let x: std::result::Result<Index,_> = serde_json::from_str(e1);
        assert!(x.is_err());

        // index too large - we must catch this in order to uphold the invariants of the index
        let e1 = r#"[["a","b"],[[0],[0,1],[0],[0,2]]]"#;
        let x: std::result::Result<Index,_> = serde_json::from_str(e1);
        assert!(x.is_err());
    }

    const STRINGS: &'static [&'static str] = &["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"];

    #[derive(Clone, PartialOrd, Ord, PartialEq, Eq)]
    struct IndexString(&'static str);

    impl Arbitrary for IndexString {

        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            IndexString(STRINGS.choose(g).unwrap())
        }
    }

    impl Arbitrary for Index {

        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let xs: Vec<BTreeSet<IndexString>> = Arbitrary::arbitrary(g);
            let xs: Vec<BTreeSet<&str>> = xs.iter().map(|e| e.iter().map(|x| x.0).collect()).collect();
            Index::from_elements(&xs)
        }
    }

    quickcheck! {
        fn serde_json_roundtrip(index: Index) -> bool {
            let json = serde_json::to_string(&index).unwrap();
            let index2: Index = serde_json::from_str(&json).unwrap();
            index == index2
        }
    }
}

fn main() {
    let index = Index::from_elements(&[
        btreeset! {"a"},
        btreeset! {"a", "b"},
        btreeset! {"a"},
        btreeset! {"a", "b"},
    ]);
    let text = serde_json::to_string(&index).unwrap();
    println!("{:?}", index);
    println!("{}", text);
    let expr = l("a") | l("b");
    println!("{:?}", index.matching(expr.dnf()));
    let expr = l("a") & l("b");
    println!("{:?}", index.matching(expr.dnf()));
    let expr = l("c") & l("d");
    println!("{:?}", index.matching(expr.dnf()));
}
