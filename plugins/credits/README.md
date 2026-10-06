# Credits

Credits is a plug-in for MediaCopy 3000. It puts the names of the crew into each manifest that a
job writes. The release packages of MediaCopy 3000 ship it. This file tells how to install it from
the source code, so that a local build of MediaCopy 3000 finds it.

MediaCopy 3000 looks for plug-ins in a folder for your account. Each plug-in has its own folder,
named after its id. The id of Credits is `tech.floreal.credits`. The folder holds the file
`plugin.json` and the program at the path that `plugin.json` gives.

| System | Folder for your account |
|---|---|
| Linux | `~/.local/share/mediacopy3000/plugins/tech.floreal.credits/` |
| macOS | `~/Library/Application Support/MediaCopy3000/Plugins/tech.floreal.credits/` |
| Windows | `%APPDATA%\MediaCopy3000\plugins\tech.floreal.credits\` |

If a release of MediaCopy 3000 also holds Credits, the folder for your account wins.

## Install

On Linux and macOS, `just install-credits` does the steps below. On Windows, do them by hand.

Do these steps from the root of the repository.

1. Build the program:

   ```
   cabal build exe:credits
   ```

2. Make the folder of the plug-in:

   ```
   mkdir -p ~/.local/share/mediacopy3000/plugins/tech.floreal.credits/bin
   ```

3. Copy `plugin.json` into the folder:

   ```
   cp plugins/credits/plugin.json ~/.local/share/mediacopy3000/plugins/tech.floreal.credits/
   ```

4. Put the program at `bin/credits` in the folder. A symbolic link to the build output is enough:

   ```
   ln -sf "$(cabal list-bin exe:credits)" ~/.local/share/mediacopy3000/plugins/tech.floreal.credits/bin/credits
   ```

   With a link, each `cabal build exe:credits` updates the plug-in. On Windows, copy the program
   to `bin\credits.exe` instead.

5. Start MediaCopy 3000. Open the main menu and select **Plug-ins**. The row **Credits** is in the
   list.

If the row is in the group **Not Valid**, the subtitle gives the reason. `mediacopy3000 plan`
writes the same reason on the error output. The usual causes are a folder with a different name
than `tech.floreal.credits`, and no file at `bin/credits`.

## Enable

A plug-in that you install is off.

1. Turn on the switch in the row **Credits**.
2. Click the row **Credits**. Its page opens.
3. In **Permissions**, for the capability `manifest.write`, select **Granted**.
4. In **Settings**, in the row **Authors**, click **Add**. Expand the row **New author**, type a role,
   then click the apply button. Credits needs at least one author with a name for a job.
5. If you installed the plug-in while the preferences were open, click **Look Again**.

The chapter *Plug-ins* of the manual, in `manual/en/plugins.md`, describes the setting **Authors**,
the job fields, and the `author` elements that Credits puts into the manifest.
