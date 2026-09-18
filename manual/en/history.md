# History and generations

A folder's history is its `ascmhl` folder. It holds one chain file and one
manifest for each time a job recorded that folder. Each job adds one generation
at the end. No job edits an earlier generation.

![The history of a folder with three generations](images/verify-history.png)

## What the expander shows

The detail pane of a verify job or a seal job holds a **History** row. Click it to open it. Each row
inside is one generation:

```
0002 · 2026-09-12 13:03
studio-01 — mediacopy3000 0.1.0.0 · xxh64 · transfer
```

A generation with failures ends its second line with ` · 2 failures`.

| The row says | What it means |
|---|---|
| `0002` | The generation number. Generations are numbered from 1 and never reused. |
| `2026-09-12 13:03` | When the generation was written. |
| `studio-01` | The computer that wrote it. |
| `mediacopy3000 0.1.0.0` | The tool that wrote it, and its version. |
| `xxh64` | The hash format of that generation. |
| `transfer` | What was done. |
| ` · 2 failures` | How many files did not agree. The row turns red. |

An offload has no such row. Its destination carries the media source's history and one new
generation, and a verify of the destination shows them. A resumed
offload carries the same history as a fresh one.

## What a process word means

The specification allows three, and no others:

| Word | What it means |
|---|---|
| `in-place` | The files were hashed where they already were. A seal and a verify both write this. |
| `transfer` | The files were copied into this folder, and the copies were hashed here. |
| `flatten` | The files were copied into one folder without their directory structure. |

## Why the chain matters

The chain file is `ascmhl/ascmhl_chain.xml`. It names every generation in order, and it holds a `c4`
hash of each manifest file.

The chain, not the folder listing, says what the history holds. The `c4` hash is there so that a
reader can tell whether a manifest was changed after it was written.

MediaCopy 3000 writes that hash, and fills it in for an older entry that has none. It does not
compare a manifest against it when it reads a history. Another ASC MHL tool can make that check.

MediaCopy 3000 refuses a history whose chain names a manifest that is not there, and one whose chain
cannot be read. Both are blockers. The [Findings](findings.md) page lists them.
