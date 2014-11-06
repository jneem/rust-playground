#[deriving(Clone, Show)]
struct Building {
    m: f64,
    b: f64,
    end: f64
}

// To prevent numerical instability, we don't allow large slopes.
static MAX_SLOPE: f64 = 1e3;

impl Building {
    fn from_points(x1: f64, y1: f64, x2: f64, y2: f64) -> Building {
        // To avoid NaNs, we deal with vertical line segments separately.
        if x1 == x2 {
            return Building {
                m: 0.0,
                b: y1.max(y2),
                end: x1
            }
        }

        let m_orig = (y2 - y1) / (x2 - x1);
        let m = m_orig.max(-MAX_SLOPE).min(MAX_SLOPE);
        let b = (y1 - m*x1).max(y2 - m*x2);
        Building { m: m, b: b, end: x1.max(x2) }
    }

    fn intersection(&self, other: &Building) -> f64 {
        let x = (other.b - self.b) / (self.m - other.m);
        if x.is_nan() { Float::neg_infinity() } else { x }
    }

    fn conceals(&self, other: &Building, x: f64) -> bool {
        self.conceals_with_intersect(other, x, self.intersection(other))
    }

    fn conceals_with_intersect(&self,
                               other: &Building,
                               x: f64,
                               intersect: f64) -> bool {
        if self.m == other.m {
            self.b >= other.b
        } else {
            (intersect <= x && self.m > other.m)
                || (intersect > x && self.m < other.m)
        }
    }

    fn empty(end: f64) -> Building {
        Building {
            m: 0.0,
            b: Float::neg_infinity(),
            end: end
        }
    }

    fn chop(&self, new_end: f64) -> Building {
        Building {
            m: self.m,
            b: self.b,
            end: new_end
        }
    }

    fn y(&self, x: f64) -> f64 {
        // We assume that the slope is not infinite. Then
        // the only way to get NaN out of m*x + b is if
        // b is infinite. But if b is infinite
        // then it should be negative infinity, and we just return it.
        if self.b.is_infinite() { self.b } else { self.m * x + self.b }
    }
}

// FIXME: the parameter of type Option<Self> is a work-around
// for not having UFCS. See
// https://mail.mozilla.org/pipermail/rust-dev/2014-May/009850.html
pub trait Direction {
    fn direction_multiplier(_: Option<Self>) -> f64;
}

#[deriving(Show)]
pub struct Up;

#[deriving(Show)]
pub struct Down;

#[deriving(Show)]
pub struct Left;

#[deriving(Show)]
pub struct Right;

impl Direction for Up { fn direction_multiplier(_: Option<Up>) -> f64 { 1.0 } }
impl Direction for Down { fn direction_multiplier(_: Option<Down>) -> f64 { -1.0 } }
impl Direction for Left { fn direction_multiplier(_: Option<Left>) -> f64 { -1.0 } }
impl Direction for Right { fn direction_multiplier(_: Option<Right>) -> f64 { 1.0 } }

pub trait Flip<T> {}
impl Flip<Up> for Down {}
impl Flip<Down> for Up {}
impl Flip<Right> for Left {}
impl Flip<Left> for Right {}

#[deriving(Clone, Show)]
pub struct Skyline<T: Direction> {
    buildings: Vec<Building>
}

impl<T: Direction> Skyline<T> {
    pub fn empty() -> Box<Skyline<T>> {
        box Skyline {
            buildings: vec![Building::empty(Float::infinity())]
        }
    }

    pub fn single(x1: f64, y1: f64, x2: f64, y2: f64) -> Box<Skyline<T>> {
        let mult = Direction::direction_multiplier(None::<T>);
        let b = Building::from_points(x1, y1 * mult, x2, y2 * mult);
        let start = Building::empty(x1.min(x2));
        let end = Building::empty(Float::infinity());

        box Skyline {
            buildings: vec![start, b, end]
        }
    }

    #[cfg(test)]
    fn from_buildings(bldgs: Vec<Building>) -> Box<Skyline<T>> {
        box Skyline {
            buildings: bldgs
        }
    }

    pub fn overlap<S: Flip<T>>(&self, other: &Skyline<S>) -> f64 {
        let mut dist: f64 = Float::neg_infinity();
        let mut start: f64 = Float::neg_infinity();
        let mut i = 0u;
        let mut j = 0u;
        let imax = self.buildings.len();
        let jmax = other.buildings.len();

        while i < imax && j < jmax {
            // Loop invariant: b1 and b2 start at or after `start`.
            let b1 = self.buildings[i];
            let b2 = other.buildings[j];

            let end: f64;
            if b1.end < b2.end {
                end = b1.end;
                i += 1;
            } else {
                end = b2.end;
                j += 1;
            }

            dist = dist.max(b1.y(start) + b2.y(start));
            dist = dist.max(b1.y(end) + b2.y(end));

            start = end;
        }

        dist
    }

    fn first_intersection(b: &Building,
                          bldgs: &[Building],
                          mut start: f64,
                          idx: &mut uint) -> f64 {
        let idxmax = bldgs.len();
        while *idx < idxmax {
            let other = &bldgs[*idx];
            let intersect = b.intersection(other);
            if b.conceals_with_intersect(other, start, intersect) {
                if intersect > start && intersect < b.end.min(other.end) {
                    // This building intersects with the other one.
                    return intersect;
                } else if b.end < other.end {
                    // This building ends before the other one.
                    return b.end;
                } else {
                    // The other building ends first (or they end together).
                    *idx += 1;
                    start = other.end;
                }
            } else {
                return start;
            }
        }
        return Float::infinity();
    }

    fn internal_merge(in1: &[Building],
                      in2: &[Building],
                      out: &mut Vec<Building>) {
        let mut start: f64 = Float::neg_infinity();
        let mut i = 0u;
        let mut j = 0u;
        let imax = in1.len();
        let jmax = in2.len();

        // Loop invariant: if j == jmax then i == imax-1.
        while i < imax && j < jmax {
            let b1 = &in1[i];
            let b2 = &in2[j];

            if b1.conceals(b2, start) {
                start = Skyline::<T>::first_intersection(b1, in2, start, &mut j);
                out.push(b1.chop(start));

                // If i == imax-1 then b1.end == inf. If in addition,
                // start >= b1.end then we must have j == jmax-1
                // (i.e., we're done with with input skylines).
                if start >= b1.end {
                    i += 1;
                }
            } else {
                start = Skyline::<T>::first_intersection(b2, in1, start, &mut i);
                out.push(b2.chop(start));
                if start >= b2.end {
                    j += 1;
                }
            }
        }
    }

    pub fn merge(&mut self, other: &Skyline<T>) {
        let mut new_bldgs = Vec::new();
        Skyline::<T>::internal_merge(self.buildings.as_slice(),
                                     other.buildings.as_slice(),
                                     &mut new_bldgs);

        self.buildings = new_bldgs;
    }

    pub fn slide(&mut self, x: f64) {
        for b in self.buildings.iter_mut() {
            b.end += x
        }
    }

    pub fn bump(&mut self, y: f64) {
        let y = y * Direction::direction_multiplier(None::<T>);
        for b in self.buildings.iter_mut() {
            b.b += y
        }
    }
}

#[cfg(test)]
mod test {
    use test_utils::ApproxEq;
    mod test_utils;

    impl<'a> ApproxEq for &'a Building {
        fn approx_eq<'b>(self, other: &'b Building) -> bool {
            self.m.approx_eq(other.m) &&
                self.b.approx_eq(other.b) &&
                self.end.approx_eq(other.end)
        }
    }

    impl<'a, T: Direction> ApproxEq for &'a Skyline<T> {
        fn approx_eq<'b>(self, other: &'b Skyline<T>) -> bool {
            if self.buildings.len() != other.buildings.len() {
                return false;
            }

            for i in range(0, self.buildings.len()) {
                if !self.buildings[i].approx_eq(&other.buildings[i]) {
                    return false;
                }
            }
            return true;
        }
    }

    #[test]
    fn basic_skyline_merge() {
        let mut sky1 = Skyline::<Up>::single(-2.0, 0.0, -1.0, 0.0);
        let mut sky2 = Skyline::<Up>::single(1.0, 0.0, 2.0, 0.0);
        sky2.merge(&*sky1);

        let target = Skyline::from_buildings(
            vec!(Building::empty(-2.0),
                 Building { m: 0.0, b: 0.0, end: -1.0 },
                 Building::empty(1.0),
                 Building { m: 0.0, b: 0.0, end: 2.0 },
                 Building::empty(Float::infinity())));

        assert!(sky2.approx_eq(&*target));
        sky1.merge(&*sky2);
        assert!(sky1.approx_eq(&*target));
    }

    #[test]
    fn basic_skyline_overlap() {
        let sky1 = Skyline::<Up>::single(-1.0, 3.0, 1.0, 3.0);
        let sky2 = Skyline::<Down>::single(-1.0, 2.0, 1.0, 2.0);

        let d = sky1.overlap(&*sky2);
        assert!(d.approx_eq(1.0), "d = {}, should be 1.0", d);
    }

    // TODO: once compilefail tests are available, add some to make
    // sure we can't compare skylines with different directions.

    // TODO: test slide and bump
}
