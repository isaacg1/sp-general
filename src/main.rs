#[macro_use]
extern crate itertools;

use std::cmp::Ordering;

#[derive(PartialEq, PartialOrd)]
enum Wrap {
    Val(f64),
}

impl Eq for Wrap {}
impl Ord for Wrap {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (&Wrap::Val(ref s), &Wrap::Val(ref o)) => s.partial_cmp(o).unwrap(),
        }
    }
}

#[derive(Clone, Debug)]
enum Placement {
    Serial(f64, usize),
    Parallel(f64, Vec<usize>),
}

#[derive(Clone, Debug)]
struct Layout {
    processors: usize,
    // Sorted
    serials: Vec<f64>,
    next_serial_index: usize,
    parallels: Vec<f64>,
    next_parallel_index: usize,
    placements: Vec<Placement>,
    cost: f64,
    profile: Vec<f64>,
}

impl Layout {
    fn new(processors: usize, serials: Vec<f64>, parallels: Vec<f64>) -> Self {
        Layout {
            processors: processors,
            serials: serials,
            next_serial_index: 0,
            parallels: parallels,
            next_parallel_index: 0,
            placements: vec![],
            cost: 0.0,
            profile: vec![0.0; processors],
        }
    }

    fn successors(&self) -> Vec<Self> {
        let (can_place_serial, can_place_parallel) =
            if let Some(next_parallel) = self.parallels.get(self.next_parallel_index) {
                if self.next_serial_index < self.serials.len() {
                    let num_bigger_serials_already_placed = self.serials
                        [..self.next_serial_index]
                        .iter()
                        .filter(|&s| s > next_parallel)
                        .count();
                    let num_smaller_serials_not_yet_placed = self.serials
                        [self.next_serial_index..]
                        .iter()
                        .filter(|&s| s < next_parallel)
                        .count();
                    (
                        num_bigger_serials_already_placed < self.processors - 1,
                        num_smaller_serials_not_yet_placed < self.processors,
                    )
                } else {
                    (false, true)
                }
            } else {
                (self.next_serial_index < self.serials.len(), false)
            };
        let mut succs = vec![];
        let mut sorted_profile_indexes = (0..self.processors).collect::<Vec<usize>>();
        sorted_profile_indexes.sort_by_key(|&i| Wrap::Val(self.profile[i]));
        if can_place_serial {
            let mut new = self.clone();
            let size = new.serials[new.next_serial_index];
            new.next_serial_index += 1;
            let index = sorted_profile_indexes[0];
            new.placements.push(Placement::Serial(size, index));
            new.profile[index] += size;
            new.cost += new.profile[index];
            succs.push(new);
        }
        if can_place_parallel {
            let size = self.parallels[self.next_parallel_index];
            for width in 1..self.processors + 1 {
                let mut new = self.clone();
                new.next_parallel_index += 1;
                let indexes = &sorted_profile_indexes[..width];
                new.placements.push(
                    Placement::Parallel(size, indexes.to_vec()),
                );
                let new_height = (indexes.iter().map(|&index| new.profile[index]).sum::<f64>() +
                                      size) /
                    width as f64;
                new.cost += new_height;
                for &index in indexes {
                    new.profile[index] = new_height;
                }
                succs.push(new);
            }
        }
        succs
    }

    fn dominates(&self, other: &Self) -> bool {
        assert_eq!(self.processors, other.processors);
        if self.next_parallel_index == other.next_parallel_index && self.next_serial_index == other.next_serial_index {
        // No removing duplicates
        let cost_better = self.cost - other.cost < -1e-6;
        let cost_as_good = self.cost - other.cost < 1e-6;

        let mut sorted_self = self.profile.clone();
        sorted_self.sort_by_key(|&p| Wrap::Val(p));
        let mut sorted_other = other.profile.clone();
        sorted_other.sort_by_key(|&p| Wrap::Val(p));
        let diffs = sorted_self.into_iter().zip(sorted_other).map(|(s,o)|s-o);
        let mut diffs_as_good = true;
        let mut a_diff_better = false;
        let mut partial_sum = 0.0;
        for diff in diffs {
            partial_sum += diff;
            let good_enough = partial_sum < 1e-6;
            let better = partial_sum < -1e-6;
            diffs_as_good &= good_enough;
            a_diff_better |= better;
        }
        diffs_as_good && (cost_better || cost_as_good && a_diff_better)
        } else {
            false
        }
    }
}

fn opt_layout(processors: usize, serials: &Vec<f64>, parallels: &Vec<f64>) -> Layout {
    let mut pool = vec![Layout::new(processors, serials.clone(), parallels.clone())];
    for i in 0..serials.len() + parallels.len() {
        //println!("Pool: {:?}", pool.iter().map(|layout|layout.placements.clone()).collect::<Vec<_>>());
        let mut new_pool: Vec<Layout> = vec![];
        for layout in &pool {
            new_pool.extend(layout.successors());
        }
        println!("{}th fresh pool size: {}", i, new_pool.len());
        let mut removal_indexes: Vec<usize> = vec![];
        for (layout1, (index, layout2)) in
            iproduct!(
                new_pool.clone().into_iter(),
                new_pool.clone().into_iter().enumerate()
            )
        {
            if layout1.dominates(&layout2) {
                removal_indexes.push(index)
            }
        }
        pool = new_pool
            .into_iter()
            .enumerate()
            .filter(|&(i, _)| !removal_indexes.contains(&i))
            .map(|(_, l)| l)
            .collect();
        println!("{}th pruned pool size: {}", i, pool.len());
    }
    pool.into_iter()
        .min_by_key(|layout| Wrap::Val(layout.cost))
        .unwrap()
}


fn main() {
    let serials = (0..20).map(|i|(i*2 + 1) as f64).collect();
    let parallels = (0..20).map(|i|(i*2 + 2) as f64).collect();
    let processors = 3;
    let layout = opt_layout(processors, &serials, &parallels);
    println!("Placements: {:?}", layout.placements);
    println!("Cost: {}", layout.cost);
}
