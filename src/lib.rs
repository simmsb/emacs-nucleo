use std::sync::Arc;

use emacs::{defun, Env, IntoLisp, Result, Value};
use nucleo::{Matcher, Nucleo};

emacs::plugin_is_GPL_compatible!();

struct NucleoSearcher {
    nucleo: Nucleo<u32>,
    matcher: Matcher,
}

#[emacs::module(name = "nucleo-module")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[defun(user_ptr)]
fn new(
    normalize: Value,
    ignore_case: Value,
    prefer_prefix: Value,
    path_matching: Value,
) -> Result<NucleoSearcher> {
    let mut config = nucleo::Config::DEFAULT;
    config.normalize = normalize.is_not_nil();
    config.ignore_case = ignore_case.is_not_nil();
    config.prefer_prefix = prefer_prefix.is_not_nil();
    if path_matching.is_not_nil() {
        config.set_match_paths();
    }

    let notify = Arc::new(move || {});

    let nucleo = nucleo::Nucleo::new(config.clone(), notify, None, 1);
    let matcher = nucleo::Matcher::new(config);
    let searcher = NucleoSearcher { nucleo, matcher };

    Ok(searcher)
}

#[defun]
fn tick<'e>(env: &'e Env, nucleo: &mut NucleoSearcher, timeout: u64) -> Result<Value<'e>> {
    let status = nucleo.nucleo.tick(timeout);

    env.list((status.running, status.changed))
}

#[defun]
fn feed(nucleo: &mut NucleoSearcher, mut strings: Value) -> Result<()> {
    let injector = nucleo.nucleo.injector();
    let mut idx = 0;

    while strings.is_not_nil() {
        let string: String = strings.car()?;
        injector.push(idx, move |_x, c| {
            c[0] = string.into();
        });
        strings = strings.cdr()?;
        idx += 1;
    }

    Ok(())
}

#[defun]
fn set_search(nucleo: &mut NucleoSearcher, pattern: String) -> Result<()> {
    nucleo.nucleo.pattern.reparse(
        0,
        &pattern,
        nucleo::pattern::CaseMatching::Smart,
        nucleo::pattern::Normalization::Smart,
        false,
    );

    Ok(())
}

fn spanify(indices: &[u32]) -> Vec<(u32, u32)> {
    let mut r = Vec::new();
    let mut it = indices.iter().copied();

    let Some(mut span_start) = it.next() else {
        return r;
    };
    let mut last_seen = span_start;
    let mut pushed = false;
    while let Some(n) = it.next() {
        if n > last_seen + 1 {
            r.push((span_start, last_seen));
            pushed = true;

            span_start = n;
            last_seen = n;
        } else {
            last_seen = n;
            pushed = false;
        }
    }

    if !pushed {
        r.push((span_start, last_seen));
    }

    r
}

fn make_list<'e, I: DoubleEndedIterator<Item = Value<'e>>>(
    env: &'e Env,
    it: I,
) -> Result<Value<'e>> {
    let mut result = false.into_lisp(env)?;

    for item in it.rev() {
        result = env.cons(item, result)?;
    }

    Ok(result)
}

#[defun]
fn results<'e>(env: &'e Env, nucleo: &mut NucleoSearcher, mut input_values: Value<'e>) -> Result<Value<'e>> {
    let snapshot = nucleo.nucleo.snapshot();
    let pattern = snapshot.pattern().column_pattern(0);
    let matcher = &mut nucleo.matcher;

    let mut indices = Vec::new();

    let mut results = Vec::new();

    for item in snapshot.matched_items(0..snapshot.matched_item_count()) {
        let s = &item.matcher_columns[0];
        indices.clear();

        let Some(score) = pattern.indices(s.slice(..), matcher, &mut indices) else {
            continue;
        };

        // indices is all the grapheme indexes which were matched
        indices.sort_unstable();
        indices.dedup();

        let spans = spanify(&indices);

        // println!("{s} {indices:?}: {spans:?}");

        results.push((score, *item.data, spans));
    }

    // initially sort by input strings index
    results.sort_by_key(|x| x.1);

    let mut input_idx = 0;

    let mut results = results.into_iter()
                         .map(|(score, idx, spans)| {
                             while input_idx < idx {
                                 input_idx += 1;
                                 input_values = input_values.cdr().unwrap();
                             }

                             let string: Value = input_values.car().unwrap();
                             (score, string, spans)
                         }).collect::<Vec<_>>();

    results.sort_by_key(|x| x.0);

    let result = make_list(
        env,
        results.into_iter().rev().filter_map(|(score, value, spans)| {
            let score = score.into_lisp(env).ok()?;
            let spans = make_list(
                env,
                spans
                    .into_iter()
                    .filter_map(|(start, end)| env.cons(start, end).ok()),
            )
            .ok()?;

            env.list((score, value, spans)).ok()
        }),
    )?;

    Ok(result)
}

#[defun]
fn say_hello(env: &Env, name: String) -> Result<Value<'_>> {
    env.message(&format!("Hello, {}!", name))
}
