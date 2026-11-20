# diff-protocol

Diffs as values for [cl-stack](https://github.com/egao1980/cl-stack) (`stack-diff`). A diff is a tree of changes (`:add` `:remove` `:modify` `:move`) with resource-string footprints and lazily loaded before/after content, computed by an engine per content type. Diffs are applied, inverted, composed, rebased and three-way merged as pure functions; conflicts from merges are data.

Brief: [`cl-stack/docs/capabilities/diff.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/diff.md). Plan: Track R, milestone R0.

Version **0.4.0** — milestones R0 and R1; 0.4.0 exports `make-conflict` for engines (#9); 0.3.0 exports the engine extension API (below, #6) and renders non-text tree files through their engine's unified form (#7); 0.2.1 adds C-quoted paths (`"a\tb"`, `\ooo` octets) and `copy from` / `copy to` sections to `:unified` (#4). R0: diff values (lazy content, footprints, levels), the sexp/plist engine, apply / invert / compose / rebase / three-way merge with restarts, `render-diff` / `parse-diff` for `:summary` and `:sexp`. R1: the `:text` engine (Myers, or git-style histogram; hunks with `text:/s-e` footprints; diff3 merge and `merge-texts` conflict markers), plain-data trees and the `:tree` engine (rename detection, per-leaf engine dispatch, mode changes), `:unified` render and a parser for `git diff` / `diff -u` output, and list-index rebasing in sexp merges (#2). Lisp-form, JSON and schema engines are later milestones.

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

## Engine extension API (0.3)

An engine in another repo (first one: [diff-backend-lisp-source](https://github.com/egao1980/diff-backend-lisp-source), `:lisp`) subclasses `diff-engine`, registers one shared instance and specializes exported generic functions only. `apply-diff` (with its restarts), `invert-diff`, `compose-diffs`, `three-way-diff`, `rebase-diff`, `parse-diff :sexp`, the tree engine's per-file nesting and `render-diff` then work unchanged.

| Hook | Called by | Default |
|------|-----------|---------|
| `(register-diff-engine name engine)` → instance | — (`find-diff-engine`, `parse-diff :sexp`, tree `:leaf-engines` names) | first registration of a name wins |
| `engine-name`, `compute-diff` (`&key level context`, accept `&allow-other-keys`), `state-equal` | everything | none |
| `(apply-change engine state change diff)` | `apply-diff`, tree leaves | none |
| `(put-value engine state change value diff)` | `apply-diff`'s `use-value` restart | none |
| `(invert-change change diff)` | `invert-diff` | swap kind / sides / move paths |
| `(merge-states engine base ours theirs resolve)` | `three-way-diff`, tree per-file merge, `rebase-by-merge` | leaf pairing by footprint |
| `(rebase-states engine diff onto base)` | `rebase-diff` | absolute-path leaves; `rebase-by-merge` for shifting paths |
| `(render-unified-content engine before after changes stream)` | `render-diff :unified` (the engine's diffs, tree files of its type) | `nil` → `Binary files … differ` / `unsupported-content` |
| `(make-conflict ours theirs)` → `diff-conflict` | an engine's `merge-states` / `rebase-states` | footprint = sorted union of both changes' footprints |

Footprints are `"<engine-name>:/…"` strings; the tree splices the file path in after the scheme and finds a leaf change's engine by that scheme. The 0.2 internal names `%apply-change`, `%put-value`, `%invert-change`, `%three-way`, `%rebase`, `%rebase-by-merge`, `%register-engine` and `%conflict` (= `make-conflict`, 0.4) name the same functions (methods on either name are one method set).

## License

MIT
