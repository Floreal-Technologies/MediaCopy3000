<script setup>
import catLatte from './images/themes/queue-catppuccin-light-latte.png'
import catFrappe from './images/themes/queue-catppuccin-dark-frappé.png'
import catMacchiato from './images/themes/queue-catppuccin-dark-macchiato.png'
import catMocha from './images/themes/queue-catppuccin-dark-mocha.png'
import alucard from './images/themes/queue-dracula-light-alucard.png'
import dracula from './images/themes/queue-dracula-dark-dracula.png'
import evLightHard from './images/themes/queue-everforest-light-hard.png'
import evLightMedium from './images/themes/queue-everforest-light-medium.png'
import evLightSoft from './images/themes/queue-everforest-light-soft.png'
import evDarkHard from './images/themes/queue-everforest-dark-hard.png'
import evDarkMedium from './images/themes/queue-everforest-dark-medium.png'
import evDarkSoft from './images/themes/queue-everforest-dark-soft.png'
import kanaLotus from './images/themes/queue-kanagawa-light-lotus.png'
import kanaDragon from './images/themes/queue-kanagawa-dark-dragon.png'
import kanaWave from './images/themes/queue-kanagawa-dark-wave.png'
</script>

# Appearance

Open the preferences with the menu item **Preferences**, or with `Ctrl+,`.

![The preferences dialog](images/preferences.png)

## Theme

The `Appearance` group holds three settings:

| Setting | What it does |
|---|---|
| `Base` | Says who decides between light and dark. |
| `Light palette` | The palette the window wears while the base is light. |
| `Dark palette` | The palette the window wears while the base is dark. |

`Base` has three values:

| Value | What it does |
|---|---|
| `Follow desktop` | The desktop decides. A change to the desktop reaches the window at once, and a running job does not delay it. |
| `Always light` | The window stays light, whatever the desktop does. |
| `Always dark` | The window stays dark, whatever the desktop does. |

Under a held base, one of the two palette rows has nothing to say. That row goes
grey, and you cannot change it until the base can reach it again.

Each palette list holds the palettes of its own base, and no other. A row gives
the name of the palette alone, because the row above it already gives the base.
The list has one section for each family, and the section header gives the
family.

| List | Sections and rows |
|---|---|
| Light palette | `System` → `System light`; `Catppuccin` → `Latte`; `Dracula` → `Alucard`; `Everforest` → `Hard`, `Medium`, `Soft`; `Kanagawa` → `Lotus` |
| Dark palette | `System` → `System dark`; `Catppuccin` → `Frappé`, `Macchiato`, `Mocha`; `Dracula` → `Dracula`; `Everforest` → `Hard`, `Medium`, `Soft`; `Kanagawa` → `Dragon`, `Wave` |

A family appears in a list only if it holds a stylesheet of that base.

![The window under the Dracula palette](images/themes/queue-dracula-dark-dracula.png)

## Where the list comes from

The application reads the list at each start. It holds no palette name of its own.

```
assets/themes/<family>/<light|dark>/<name>.css
```

The first directory gives the family, the second gives the base, and the name of the file gives the
rest of the row. A dash in a file name becomes a space, and each word starts with a capital letter. A directory
with an other name than `light` or `dark` holds no palette, and the application passes it by.

To add a palette, put a CSS file at that place. To remove one, delete its file.

## What a family says about itself

A family can hold one JSON file beside its `light` and `dark` directories:

```
assets/themes/<family>/family.json
```

```json
{
  "name": "Catppuccin",
  "homepage": "https://catppuccin.com"
}
```

| Field | What it does |
|---|---|
| `name` | The name in the section header. Without it, the header takes the name of the directory. |
| `homepage` | The address of the family's own page. With it, the header shows a link button at its right; push the button to open the page in the browser. |

Both fields are optional, and the file itself is optional. If the file cannot be parsed, only metadata are lost;
the palettes stay in the list, and the application writes the reason of the parser on its error output.
The `System` header has no link.

| Family | Page |
|---|---|
| `Catppuccin` | <https://catppuccin.com> |
| `Dracula` | <https://draculatheme.com> |
| `Everforest` | <https://everforest.vercel.app> |
| `Kanagawa` | <https://github.com/rebelot/kanagawa.nvim> |

## Gallery

One picture of the queue for each palette, in the order the lists show them. Click a picture to open
it in a new tab.

### [Catppuccin](https://catppuccin.com)

#### Light

<div class="theme-gallery">
  <figure>
    <a :href="catLatte" target="_blank" rel="noopener">
      <img src="./images/themes/queue-catppuccin-light-latte.png" alt="The queue under Catppuccin Latte, light mode">
    </a>
    <figcaption>Latte</figcaption>
  </figure>
</div>

#### Dark

<div class="theme-gallery">
  <figure>
    <a :href="catFrappe" target="_blank" rel="noopener">
      <img src="./images/themes/queue-catppuccin-dark-frappé.png" alt="The queue under Catppuccin Frappé, dark mode">
    </a>
    <figcaption>Frappé</figcaption>
  </figure>
  <figure>
    <a :href="catMacchiato" target="_blank" rel="noopener">
      <img src="./images/themes/queue-catppuccin-dark-macchiato.png" alt="The queue under Catppuccin Macchiato, dark mode">
    </a>
    <figcaption>Macchiato</figcaption>
  </figure>
  <figure>
    <a :href="catMocha" target="_blank" rel="noopener">
      <img src="./images/themes/queue-catppuccin-dark-mocha.png" alt="The queue under Catppuccin Mocha, dark mode">
    </a>
    <figcaption>Mocha</figcaption>
  </figure>
</div>

### [Dracula](https://draculatheme.com)

#### Light

<div class="theme-gallery">
  <figure>
    <a :href="alucard" target="_blank" rel="noopener">
      <img src="./images/themes/queue-dracula-light-alucard.png" alt="The queue under Dracula Alucard, light mode">
    </a>
    <figcaption>Alucard</figcaption>
  </figure>
</div>

#### Dark

<div class="theme-gallery">
  <figure>
    <a :href="dracula" target="_blank" rel="noopener">
      <img src="./images/themes/queue-dracula-dark-dracula.png" alt="The queue under Dracula Dracula, dark mode">
    </a>
    <figcaption>Dracula</figcaption>
  </figure>
</div>

### [Everforest](https://everforest.vercel.app)

#### Light

<div class="theme-gallery">
  <figure>
    <a :href="evLightHard" target="_blank" rel="noopener">
      <img src="./images/themes/queue-everforest-light-hard.png" alt="The queue under Everforest Hard, light mode">
    </a>
    <figcaption>Hard</figcaption>
  </figure>
  <figure>
    <a :href="evLightMedium" target="_blank" rel="noopener">
      <img src="./images/themes/queue-everforest-light-medium.png" alt="The queue under Everforest Medium, light mode">
    </a>
    <figcaption>Medium</figcaption>
  </figure>
  <figure>
    <a :href="evLightSoft" target="_blank" rel="noopener">
      <img src="./images/themes/queue-everforest-light-soft.png" alt="The queue under Everforest Soft, light mode">
    </a>
    <figcaption>Soft</figcaption>
  </figure>
</div>

#### Dark

<div class="theme-gallery">
  <figure>
    <a :href="evDarkHard" target="_blank" rel="noopener">
      <img src="./images/themes/queue-everforest-dark-hard.png" alt="The queue under Everforest Hard, dark mode">
    </a>
    <figcaption>Hard</figcaption>
  </figure>
  <figure>
    <a :href="evDarkMedium" target="_blank" rel="noopener">
      <img src="./images/themes/queue-everforest-dark-medium.png" alt="The queue under Everforest Medium, dark mode">
    </a>
    <figcaption>Medium</figcaption>
  </figure>
  <figure>
    <a :href="evDarkSoft" target="_blank" rel="noopener">
      <img src="./images/themes/queue-everforest-dark-soft.png" alt="The queue under Everforest Soft, dark mode">
    </a>
    <figcaption>Soft</figcaption>
  </figure>
</div>

### [Kanagawa](https://github.com/rebelot/kanagawa.nvim)

#### Light

<div class="theme-gallery">
  <figure>
    <a :href="kanaLotus" target="_blank" rel="noopener">
      <img src="./images/themes/queue-kanagawa-light-lotus.png" alt="The queue under Kanagawa Lotus, light mode">
    </a>
    <figcaption>Lotus</figcaption>
  </figure>
</div>

#### Dark

<div class="theme-gallery">
  <figure>
    <a :href="kanaDragon" target="_blank" rel="noopener">
      <img src="./images/themes/queue-kanagawa-dark-dragon.png" alt="The queue under Kanagawa Dragon, dark mode">
    </a>
    <figcaption>Dragon</figcaption>
  </figure>
  <figure>
    <a :href="kanaWave" target="_blank" rel="noopener">
      <img src="./images/themes/queue-kanagawa-dark-wave.png" alt="The queue under Kanagawa Wave, dark mode">
    </a>
    <figcaption>Wave</figcaption>
  </figure>
</div>

<style scoped>
.theme-gallery {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 1rem;
  margin: 1rem 0 2rem;
}
.theme-gallery figure {
  margin: 0;
  border: 1px solid var(--vp-c-divider);
  border-radius: 8px;
  overflow: hidden;
  background: var(--vp-c-bg-soft);
}
.theme-gallery img {
  display: block;
  width: 100%;
  height: auto;
}
.theme-gallery figcaption {
  padding: 0.5rem 0.75rem;
  font-size: 0.9rem;
  text-align: center;
}
@media (max-width: 720px) {
  .theme-gallery { grid-template-columns: 1fr; }
}
</style>
