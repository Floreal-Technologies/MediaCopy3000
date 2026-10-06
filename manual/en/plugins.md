# Plug-ins

A plug-in adds work to a job. It is a separate program. MediaCopy 3000 starts it for each job and
sends it messages.

A plug-in never changes a media file. It never writes a manifest. It gives data to MediaCopy 3000,
and MediaCopy 3000 checks the data and writes it.

## What a plug-in can do

A plug-in has one or more roles.

| Role | When it runs | What it does |
|---|---|---|
| Inspector | When the plan is made, and after each file is verified | Adds findings to the plan. Adds notes about each file to the report. |
| Contributor | When the plan is made | Adds authors and metadata to each manifest that the job writes. |

The plan sheet shows what the contributors add, in the group **Recorded in the Manifest**. What you
approve with **Start** is what the job writes. Information that a plug-in finds after the copy goes
into the report, never into the manifest.

## Install a plug-in

Each plug-in has its own folder, named after its id. The folder holds a file `plugin.json` and the
program. The id is a reverse domain name in lowercase, for example `tech.floreal.c2pa-reader`: it
holds only `a` to `z`, digits, `.`, `-` and `_`, at least one `.`, and no `..`, and it does not start
or end with `.`.

| System | Folder for your account | Folder for all accounts |
|---|---|---|
| Linux | `~/.local/share/mediacopy3000/plugins/<id>/` | `mediacopy3000/plugins/<id>/` in each folder of `XDG_DATA_DIRS`, for example `/usr/share` |
| macOS | `~/Library/Application Support/MediaCopy3000/Plugins/<id>/` | None. The folder in the app holds the plug-ins that come with MediaCopy 3000: refer to the next table. |
| Windows | `%APPDATA%\MediaCopy3000\plugins\<id>\` | `%PROGRAMDATA%\MediaCopy3000\plugins\<id>\` |
| Flatpak | `~/.var/app/tech.floreal.MediaCopy3000/data/mediacopy3000/plugins/<id>/` | The Flatpak extension `tech.floreal.MediaCopy3000.Plugin.<id>` |

Plug-ins that come with MediaCopy 3000 are in a different folder:

| System | Folder for plug-ins that come with MediaCopy 3000 |
|---|---|
| Linux | `lib/mediacopy3000/plugins/<id>/` beside the folder `bin` of the program, for example `/usr/lib/mediacopy3000/plugins/<id>/` |
| macOS | `MediaCopy3000.app/Contents/PlugIns/<id>/` |
| Windows | `%LOCALAPPDATA%\Programs\MediaCopy 3000\plugins\<id>\` |
| Flatpak | `/app/lib/mediacopy3000/plugins/<id>/` |

If two folders hold the same id, the folder for your account wins, and the folder of plug-ins that
come with MediaCopy 3000 loses. You can thus install a newer version of such a plug-in for your
account.

The page **Plug-ins** of the preferences lists each plug-in folder that is not valid, with the
reason, in the group **Not Valid**. `mediacopy3000 plan` writes the same reasons on the error output.

## Enable a plug-in

![The page Plug-ins of the preferences](images/plugins.png)

A plug-in that you install is off. It asks for capabilities, and you must answer each one.

1. Open the main menu and select **Plug-ins**.
2. Click the row of the plug-in to expand it.
3. Turn on **Enabled**.
4. For each capability, select **Granted** or **Declined**.
5. For each setting, type a value, then click the apply button at the right of the row.

To clear a setting, apply an empty value, or select **Not set**. The plug-in then gets the default
value of the setting, if it has one. The rows show the default values.

A plug-in runs only when each capability it asks for is granted or declined. A new version of a
plug-in that asks for a new capability stops until you answer it. The subtitle of the row tells why
an enabled plug-in does not run.

If you add or remove a plug-in folder while the preferences are open, click **Look Again**.

### The file `plugins.json`

MediaCopy 3000 keeps your answers and the settings in the file `plugins.json`. You can also edit
this file yourself, for example to install the same plug-ins on many computers.

1. Open the file `plugins.json`. Make it if it does not exist.

   | System | Path |
   |---|---|
   | Linux | `~/.config/mediacopy3000/plugins.json` |
   | macOS | `~/Library/Application Support/MediaCopy3000/plugins.json` |
   | Windows | `%APPDATA%\MediaCopy3000\plugins.json` |
   | Flatpak | `~/.var/app/tech.floreal.MediaCopy3000/config/mediacopy3000/plugins.json` |

2. Add an entry for the plug-in:

   ```json
   {
     "plugins": {
       "tech.floreal.c2pa-reader": {
         "enabled": true,
         "grants": ["files.read"],
         "declined": ["block"],
         "settings": {}
       }
     }
   }
   ```

3. Put each capability that the plug-in asks for in `grants` or in `declined`.
4. Put a value for each setting of the plug-in in `settings`.
5. If the preferences are open, click **Look Again**.

MediaCopy 3000 keeps the keys of `plugins.json` that it does not know when it writes the file.

## What MediaCopy 3000 enforces

| Capability | What it allows | What MediaCopy 3000 enforces |
|---|---|---|
| `files.read` | Read the media files | Nothing. The plug-in promises it. |
| `block` | An inspector can stop a job with a blocker | Without this grant, a blocker from the plug-in becomes a warning. |
| `manifest.write` | A contributor can add data to the manifest | Without this grant, MediaCopy 3000 does not ask the plug-in for data. |

A plug-in is a program that runs with your rights. MediaCopy 3000 cannot stop it from reading a file
or from opening a network connection. Install only plug-ins from a source that you trust.

## Job fields

A plug-in can ask for a value for each job, for example the name of the camera operator.

In the window, the plan sheet shows the group **Job Fields**, with one row for each field of each
enabled plug-in. Type the value, then click the apply button. MediaCopy 3000 makes the plan again
with the value. The page [The plan](the-plan.md) shows this group.

On the command line, give the value with `--plugin-field`:

```
mediacopy3000 plan offload SOURCE DEST --plugin-field tech.floreal.credits.operator="Sam Roe"
```

If a required job field has no value, the plan gives the finding
`the field operator has no valid value`. For a contributor, and for an inspector with the `block`
grant, this finding is a blocker. For any other plug-in, it is a warning, and the plug-in does not
run.

## When a plug-in fails

A plug-in fails when it does not start, stops, sends a bad answer, does not read a request, writes
a line longer than 16 MiB, or gives no sign of life for 30 seconds. A long task does not fail while
the plug-in reports its progress.

| When | Plug-in | Result |
|---|---|---|
| The plan is made | Contributor, or inspector with the `block` grant | A blocker. **Start** stays grey. |
| The plan is made | Any other plug-in | A warning. The plug-in does not take part in the job. |
| The plan is blocked | All | No plug-in starts for the job. |
| A file is inspected | Inspector | MediaCopy 3000 starts the plug-in again and sends the same file again. After a second failure, the files that are left are `not inspected`. The job result does not change. |

When you cancel a job, MediaCopy 3000 stops each plug-in, and the programs that it started, in 3
seconds or less.

## Write a plug-in

A plug-in reads messages on its standard input and writes answers on its standard output. Each
message is one JSON-RPC 2.0 object on one line. MediaCopy 3000 sends these requests:

| Method | When |
|---|---|
| `initialize` | First. It gives the settings, the job fields and the grants. |
| `inspect/plan` | When the plan is made, to an inspector |
| `contribute` | When the plan is made, to a contributor |
| `inspect/file` | After each verified file, to an inspector |
| `shutdown` | Last |

The plug-in can send the notifications `$/progress` and `$/log`. Lines on its error output go into
the event log of the job.

The file `plugin-protocol/schema/protocol-1.schema.json` in the source code describes each message
and `plugin.json`.

A plug-in must obey these rules:

- It stops when its standard input closes.
- It reads each request. MediaCopy 3000 stops a plug-in that does not read a request in the time
  limit of the call.
- It writes lines of 16 MiB or less.
- It gives the same answer to `inspect/plan` and `contribute` for the same job. MediaCopy 3000 makes
  a queued plan again when its turn comes, and a different answer sends the job back for review.
- It writes metadata only in the XML namespace that its `plugin.json` declares. This namespace
  cannot be empty, a namespace of ASC MHL, or a namespace that XML reserves. The metadata has at
  most 32 levels of elements and only characters that XML 1.0 allows. MediaCopy 3000 removes XML
  comments and processing instructions.
- It writes in an author only characters that XML 1.0 allows.
- Each path in `executable` is relative to the folder of the plug-in. It does not start with `/` or
  `\`, holds no `:`, and has no `..` part. One bad path, for any system, makes the plug-in not
  valid on every system.

MediaCopy 3000 replaces each control character and line break in a text from a plug-in, such as a
title, a label or a log line, with a space.

The event log of a job holds at most 1,024 lines that wait to be written. While it is full, new
lines are lost, and lines of MediaCopy 3000 too. A plug-in that writes many lines at once can thus
make the log lose lines.

### Trace the messages

To see each message between MediaCopy 3000 and a plug-in, turn on **Trace Messages** in the row of
the plug-in. The key `"trace": true` in `plugins.json` does the same.

Each start of the plug-in then makes one file in this folder:

| System | Path |
|---|---|
| Linux, macOS | `~/.local/state/mediacopy3000/plugin-traces/` |
| Windows | `%LOCALAPPDATA%\mediacopy3000\plugin-traces\` |
| Flatpak | `~/.var/app/tech.floreal.MediaCopy3000/.local/state/mediacopy3000/plugin-traces/` |

The name of the file is `<id>-<date>_<time>-<stage>-<pid>.jsonl`. The stage is `plan` or `run`. A
plug-in that MediaCopy 3000 starts again gets a new file.

Each line of the file is one JSON object:

- `time`: the time in UTC, to the millisecond.
- `dir`: `out` for a message to the plug-in, `in` for a line on its standard output, `stderr` for a
  line on its error output.
- `message`: the line, if it is JSON. Otherwise `text` holds the line as text.

MediaCopy 3000 does not delete trace files. Turn off **Trace Messages** when you do not need it. A
trace that cannot be written adds one line to the event log of the job, and the plug-in runs on.

## Ship a plug-in

Give the plug-in as one folder that holds `plugin.json` and the programs for each system that it
supports. The name of the folder is the id of the plug-in.

For the Flatpak version of MediaCopy 3000, ship the plug-in as a Flatpak extension:

1. Give the extension the id `tech.floreal.MediaCopy3000.Plugin.<id>`, for example
   `tech.floreal.MediaCopy3000.Plugin.tech.floreal.c2pa-reader`.
2. Install `plugin.json` and the program at the root of the extension.
3. Build the program for the runtime `org.gnome.Platform`, version 49.

A Flatpak id allows the character `-` only in its last part, and no part can start with a digit.
If the id of the plug-in breaks these rules, it cannot be the name of an extension. Then ship the
folder, and tell the person to put it in the folder for the account.

The extension appears in the sandbox as the folder `/app/share/mediacopy3000/plugins/<id>/`. The
runtime holds Python 3. A program in Python can thus run with no other files.
