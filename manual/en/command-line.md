# The command line

MediaCopy 3000 is a window. One command runs without it.

## Print a plan

```
mediacopy3000 plan offload SOURCE DEST [DEST…] [--resume | --replace] [--seal-first] [PLUG-IN OPTIONS]
mediacopy3000 plan verify FOLDER [PLUG-IN OPTIONS]
mediacopy3000 plan seal FOLDER [PLUG-IN OPTIONS]
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

## Plug-in options

The plan command starts the enabled [plug-ins](plugins.md), as the window does.

| Option | What it does |
|---|---|
| `--no-plugins` | Makes the plan with no plug-in. |
| `--plugin-field ID.KEY=VALUE` | Gives the job field `KEY` to the plug-in `ID`. Give the option once for each field. |

A blocker from a plug-in also gives the exit code `1`. The command writes a line on the error output
for each plug-in that is not valid or not enabled, and for each line that a plug-in writes on its
own error output.
