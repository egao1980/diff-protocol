# diff-protocol

Diffs as values for [cl-stack](https://github.com/egao1980/cl-stack) (`stack-diff`). A diff is a tree of changes (`:add` `:remove` `:modify` `:move`) with resource-string footprints and lazily loaded before/after content, computed by an engine per content type. Diffs are applied, inverted, composed, rebased and three-way merged as pure functions; conflicts from merges are data.

Brief: [`cl-stack/docs/capabilities/diff.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/diff.md). Plan: Track R, milestone R0.

Version **0.1.0** — milestone R0: diff values (lazy content, footprints, levels), the sexp/plist engine, apply / invert / compose / rebase / three-way merge with restarts, and `render-diff` / `parse-diff` for `:summary` and `:sexp`. Text, tree, lisp-form, JSON and schema engines are later milestones.

```lisp
(let* ((e (stack-diff:find-diff-engine :sexp))
       (d (stack-diff:compute-diff e '(:a 1 :b 2) '(:a 1 :b 3))))
  (stack-diff:render-diff d :summary nil)        ; => "M sexp:/b
                                                 ;    "
  (stack-diff:apply-diff '(:a 1 :b 2) d))        ; => (:A 1 :B 3)
```

| Piece | R0 |
|-------|----|
| Values | `change`, `opaque-change`, `diff`, `diff-conflict`; levels `:summary` `:changes` `:content` |
| Engines | `sexp-diff-engine` (atoms, plists, lists). Text / tree / lisp-form / JSON / schema engines come later |
| Protocol | `compute-diff` `state-equal` `apply-diff` `invert-diff` `compose-diffs` `rebase-diff` `three-way-diff` `render-diff` `parse-diff` |
| Formats | `:summary`, `:sexp` (canonical, plain data) |
| Conditions | `diff-error` → `irreversible-change`, `apply-conflict` (`skip-change` / `use-value` / `abort`), `unsupported-content` |

No dependencies. Tests: Rove (`cl-repo test`); laws are `deftest-parametrize` cases over a fixed corpus.

## License

MIT
