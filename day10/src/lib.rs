use num::integer::gcd;
use std::cmp::{Ord, Ordering};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Vector {
    distance_squared: u64,
    angle: Angle,
}

impl Vector {
    pub fn new(dx: i32, dy: i32) -> Vector {
        let x = dx as i64;
        let y = dy as i64;
        Vector {
            distance_squared: (x * x) as u64 + (y * y) as u64,
            angle: Angle::new(dx, dy),
        }
    }

    pub fn distance_squared(&self) -> u64 {
        self.distance_squared
    }

    pub fn angle(&self) -> Angle {
        self.angle
    }
}

impl Eq for Vector {}

impl Ord for Vector {
    fn cmp(&self, other: &Self) -> Ordering {
        let angle_cmp = self.angle.cmp(&other.angle);
        if angle_cmp == Ordering::Equal {
            self.distance_squared.cmp(&other.distance_squared)
        } else {
            angle_cmp
        }
    }
}

impl PartialOrd for Vector {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Angle {
    dx: i32,
    dy: i32,
}

impl Angle {
    pub fn new(dx: i32, dy: i32) -> Angle {
        if dx == 0 && dy == 0 {
            Angle { dx, dy }
        } else {
            let gcd = gcd(dx, dy);
            Angle {
                dx: dx / gcd,
                dy: dy / gcd,
            }
        }
    }
}

impl Eq for Angle {}

impl Ord for Angle {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.dx < 0 {
            if other.dx < 0 {
                (self.dx as i64 * other.dy as i64).cmp(&(other.dx as i64 * self.dy as i64))
            } else {
                Ordering::Greater
            }
        } else if self.dx == 0 {
            if self.dy == 0 {
                if other.dx == 0 && other.dy == 0 {
                    Ordering::Equal
                } else {
                    Ordering::Less
                }
            } else if other.dx == 0 {
                if other.dy == 0 {
                    Ordering::Greater
                } else {
                    other.dy.cmp(&self.dy)
                }
            } else if other.dx < 0 || self.dy > 0 {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        } else {
            if other.dx > 0 {
                (self.dx as i64 * other.dy as i64).cmp(&(other.dx as i64 * self.dy as i64))
            } else if other.dx == 0 && other.dy >= 0 {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        }
    }
}

impl PartialOrd for Angle {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    #[test]
    fn angle_ord() {
        let limit = 50;
        for ax in -limit..=limit {
            for ay in -limit..=limit {
                let a = Angle::new(ax, ay);
                let deg_a = if ax == 0 && ay == 0 {
                    -1
                } else {
                    ((10000.0 * (90.0 - ((ay as f64).atan2(ax as f64) * 180.0 / PI))) as i32
                        + 3600000)
                        % 3600000
                };
                for bx in -limit..=limit {
                    for by in -limit..=limit {
                        let b = Angle::new(bx, by);
                        let deg_b = if bx == 0 && by == 0 {
                            -1
                        } else {
                            ((10000.0 * (90.0 - ((by as f64).atan2(bx as f64) * 180.0 / PI)))
                                as i32
                                + 3600000)
                                % 3600000
                        };
                        if a.cmp(&b) != deg_a.cmp(&deg_b) {
                            panic!(
                                "{:?} ({}) {:?} {:?} ({}) [incorrectly gave {:?}]",
                                a,
                                deg_a,
                                deg_a.cmp(&deg_b),
                                b,
                                deg_b,
                                a.cmp(&b)
                            );
                        }
                    }
                }
            }
        }
    }
}
