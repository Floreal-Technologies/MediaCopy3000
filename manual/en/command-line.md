# The command line

MediaCopy 3000 is a window. One command runs without it.

## Print a plan

```
mediacopy3000 plan offload SOURCE DEST [DEST…] [--resume | --replace] [--seal-first]
mediacopy3000 plan verify FOLDER
mediacopy3000 plan seal FOLDER
```

The command reads the folders, prints the plan, and exits. It writes nothing. `mediacopy3000 plan --help`
prints the shapes, and `mediacopy3000 --help` prints the commands.

Every other argument goes to the toolkit, which is how the desktop file starts the window.

| Exit code | Meaning |
|---|---|
| `0` | The plan is ready. |
| `1` | A blocker stands. The plan names it. |
| `2` | The arguments are wrong, or the plan could not be made. The message is on the error output. |

`--resume` and `--replace` choose what to do with a destination that holds a partial copy. The
page [Offloading a media source](offload.md) explains both. `--seal-first` seals the media source
before the copy, and stops if the seal finds a problem.
