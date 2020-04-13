use maplit::btreeset;
use reduce::Reduce;
use std::collections::BTreeSet;
use std::ops::{BitAnd, BitOr};

struct Index {
    strings: Vec<String>,
    elements: Vec<Vec<u32>>,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
enum Expression {
    Literal(String),
    And(Vec<Expression>),
    Or(Vec<Expression>),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expression::Literal(text) => write!(f, "{}", text),
            Expression::And(es) => write!(
                f,
                "{}",
                es.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("&")
            ),
            Expression::Or(es) => write!(
                f,
                "({})",
                es.iter().map(|x| x.to_string()).collect::<Vec<_>>().join("|")
            ),
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
struct Dnf(BTreeSet<BTreeSet<String>>);

impl Dnf {
    fn literal(text: String) -> Self {
        Self(btreeset![btreeset![text]])
    }

    fn expression(self) -> Expression {
        self.0.into_iter().map(Dnf::and_expr).reduce(Expression::bitor).unwrap()
    }

    fn and_expr(v: BTreeSet<String>) -> Expression {
        v.into_iter().map(Expression::literal).reduce(Expression::bitand).unwrap()
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

impl BitOr for Dnf {
    type Output = Dnf;
    fn bitor(self, that: Self) -> Self {
        let mut es = self.0;
        es.extend(that.0);
        Dnf(es)
    }
}

fn insert_unless_redundant(
    mut aa: BTreeSet<BTreeSet<String>>,
    b: BTreeSet<String>,
) -> BTreeSet<BTreeSet<String>> {
    let mut to_remove = None;
    for a in aa.iter() {
        if a.is_subset(&b) {
            // a is larger than b. E.g. x | x&y
            // keep a, b is redundant
            return aa;
        } else if a.is_superset(&b) {
            // a is smaller than b, E.g. x&y | x
            // remove a, keep b
            to_remove = Some(&b);
        }
    }
    if let Some(r) = to_remove {
        aa.remove(r);
    }
    aa.insert(b);
    aa
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
                rs = insert_unless_redundant(rs, r);
            }
        }
        Dnf(rs)
    }
}

fn l(x: &str) -> Expression {
    Expression::literal(x.into())
}
fn main() {
    {
        let a = l("a");
        let b = l("b");
        let c = l("c");
        let d = l("d");
        let expr = (a | b) & (c | d);
        println!("{}", expr);
        let dnf = expr.dnf();
        println!("{:?} {}", dnf.clone(), dnf.expression());
    }

    {
        let a = l("a");
        let b = l("b");
        let expr = (a.clone() | b) & a;
        println!("{}", expr);
        let dnf = expr.dnf();
        println!("{:?} {}", dnf.clone(), dnf.expression());
    }

    {
        let a = l("a");
        let b = l("b");
        let expr = (a.clone() | b) | a;
        println!("{}", expr);
        let dnf = expr.dnf();
        println!("{:?} {}", dnf.clone(), dnf.expression());
    }
}
