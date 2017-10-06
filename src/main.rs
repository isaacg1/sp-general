extern crate itertools;

use itertools::Itertools;

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
    fn new(processors: usize, mut serials: Vec<f64>, mut parallels: Vec<f64>) -> Self {
        serials.sort_by_key(|&v| Wrap::Val(v));
        parallels.sort_by_key(|&v| Wrap::Val(v));
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
                    let num_bigger_serials_already_placed = self.serials[..self.next_serial_index]
                        .iter()
                        .filter(|&s| s > next_parallel)
                        .count();
                    let num_smaller_serials_not_yet_placed = self.serials[self.next_serial_index..]
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
        if can_place_serial {
            let mut new = self.clone();
            new.place_serial();
            succs.push(new);
        }
        if can_place_parallel {
            let mut new = self.clone();
            new.place_parallel();
            succs.push(new);
        }
        succs
    }

    fn place_serial(&mut self) {
        let sorted_profile_indexes = (0..self.processors).collect::<Vec<usize>>();
        let index = sorted_profile_indexes
            .into_iter()
            .min_by_key(|&i| Wrap::Val(self.profile[i]))
            .unwrap();
        let size = self.serials[self.next_serial_index];
        self.next_serial_index += 1;
        self.placements.push(Placement::Serial(size, index));
        self.profile[index] += size;
        self.cost += self.profile[index];
    }

    fn try_place_parallel(&mut self, width: usize) -> Result<(), ()> {
        let mut sorted_profile_indexes = (0..self.processors).collect::<Vec<usize>>();
        sorted_profile_indexes.sort_by_key(|&i| Wrap::Val(self.profile[i]));
        let indexes = &sorted_profile_indexes[..width];
        let size = self.parallels[self.next_parallel_index];
        let total_old_heights = indexes
            .iter()
            .map(|&index| self.profile[index])
            .sum::<f64>();
        let new_height = (total_old_heights + size) / width as f64;
        for &index in indexes {
            if new_height - self.profile[index] < -1e-6 {
                return Err(());
            }
        }
        self.next_parallel_index += 1;
        self.placements.push(
            Placement::Parallel(size, indexes.to_vec()),
        );
        self.cost += new_height;
        for &index in indexes {
            self.profile[index] = new_height;
        }
        Ok(())
    }

    fn place_parallel(&mut self) {
        for width in (1..self.processors + 1).rev() {
            let res = self.try_place_parallel(width);
            if res.is_ok() {
                break;
            }
        }
    }

    fn dominates(&self, other: &Self) -> bool {
        assert_eq!(self.processors, other.processors);
        if self.next_parallel_index == other.next_parallel_index &&
            self.next_serial_index == other.next_serial_index
        {
            let cost_diff = self.cost - other.cost;

            let mut sorted_self = self.profile.clone();
            sorted_self.sort_by_key(|&p| Wrap::Val(p));
            let mut sorted_other = other.profile.clone();
            sorted_other.sort_by_key(|&p| Wrap::Val(p));
            let diffs = sorted_self.into_iter().zip(sorted_other).map(
                |(s, o)| s - o,
            );
            let mut worst_partial_sum_or_zero = 0.0;
            let mut partial_sum = 0.0;
            for diff in diffs {
                partial_sum += diff;
                if -partial_sum < worst_partial_sum_or_zero {
                    worst_partial_sum_or_zero = -partial_sum
                }
            }
            cost_diff < worst_partial_sum_or_zero + 1e-6
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
        new_pool.sort_by_key(|elt| (elt.next_parallel_index, elt.next_serial_index));
        println!("{}th fresh pool size: {}", i, new_pool.len());
        let mut removal_indexes: Vec<usize> = vec![];
        for (_, group) in &new_pool.clone().into_iter().enumerate().group_by(
            |&(_, ref elt)| {
                (elt.next_parallel_index, elt.next_serial_index)
            },
        )
        {
            let group_vec: Vec<_> = group.collect();
            for (i1, layout1) in group_vec.clone().into_iter() {
                for (i2, layout2) in group_vec.clone().into_iter() {
                    if i1 != i2 && !removal_indexes.contains(&i1) && layout1.dominates(&layout2) {
                        removal_indexes.push(i2)
                    }
                }
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

fn srpt_layout(processors: usize, serials: &Vec<f64>, parallels: &Vec<f64>) -> Layout {
    let mut layout = Layout::new(processors, serials.clone(), parallels.clone());
    for _ in 0..serials.len() + parallels.len() {
        if layout.next_parallel_index >= layout.parallels.len() ||
            layout.next_serial_index < layout.serials.len() &&
                layout.serials[layout.next_serial_index] <
                    layout.parallels[layout.next_parallel_index]
        {
            layout.place_serial();
        } else {
            layout.place_parallel();
        }
    }
    layout
}

fn ordering_to_layout(
    ordering: &Vec<(usize, bool)>,
    processors: usize,
    serials: &Vec<f64>,
    parallels: &Vec<f64>,
) -> Layout {
    let mut layout = Layout::new(processors, serials.clone(), parallels.clone());
    for &(_index, is_parallel) in ordering {
        if is_parallel {
            layout.place_parallel();
        } else {
            layout.place_serial();
        }
    }
    layout
}

fn interchange_layout(processors: usize, serials: &Vec<f64>, parallels: &Vec<f64>) -> Layout {
    let mut ordering: Vec<(usize, bool)> = (0..serials.len())
        .map(|i| (i, false))
        .chain((0..parallels.len()).map(|i| (i, true)))
        .collect();
    ordering.sort_by_key(|&(i, is_parallel)| {
        Wrap::Val(if is_parallel {
            parallels[i]
        } else {
            serials[i]
        })
    });
    let mut changed = true;
    let mut current = ordering_to_layout(&ordering, processors, serials, parallels);
    while changed {
        changed = false;
        current = ordering_to_layout(&ordering, processors, serials, parallels);
        let mut new_ordering = ordering.clone();
        for (start_index, window) in ordering.windows(2).enumerate() {
            let prev = window[0];
            let next = window[1];

            if prev.1 != next.1 {
                new_ordering = ordering.clone();
                new_ordering[start_index] = next;
                new_ordering[start_index + 1] = prev;
                let new_layout = ordering_to_layout(&new_ordering, processors, serials, parallels);
                if new_layout.cost < current.cost {
                    changed = true;
                    println!("Swapped {:?} with {:?}", prev, next);
                    break;
                }
            }
        }
        if changed {
            ordering = new_ordering;
        }
    }
    current
}

fn main() {
    let serials = vec![2., 4., 6., 8.];
    let parallels = vec![1., 3., 5., 7.];
    let processors = 4;
    let opt_layout = opt_layout(processors, &serials, &parallels);
    println!("Placements: {:?}", opt_layout.placements);
    println!("Opt cost: {}", opt_layout.cost);
    let srpt_out = srpt_layout(processors, &serials, &parallels);
    //println!("Placements: {:?}", srpt_out.placements);
    println!("SRPT cost: {}", srpt_out.cost);
    let interchange = interchange_layout(processors, &serials, &parallels);
    println!("Interchange cost: {}", interchange.cost);
    println!(
        "SRPT-OPT/(sum serial): {}",
        (srpt_out.cost - opt_layout.cost) / (serials.iter().sum::<f64>())
    );
    println!(
        "SRPT-OPT/(sum parallel): {}",
        (srpt_out.cost - opt_layout.cost) / (parallels.iter().sum::<f64>())
    );
}
