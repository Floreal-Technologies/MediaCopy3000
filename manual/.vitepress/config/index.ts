import { defineConfig } from 'vitepress'
import en from './en'
import fr from './fr'

// The shared config. It holds only what a locale cannot hold: the rewrite, the sitemap, the base,
// and the search box. Words that belong to one language live in `en.ts` and `fr.ts`.
//
// `locales.root` is English. Its pages are in `manual/en/`, and the rewrite below serves them at
// the root, so every locale has its own folder and none of them is a special case on disk.
//
// A locale's `themeConfig` is merged one level deep, not recursively. A locale that names `search`
// would replace the whole search box, so `search` stays here and neither locale mentions it.

export default defineConfig({
  title: 'MediaCopy 3000',
  description: 'Verified media offload and ASC MHL verification for videographers',
  // A project site is served under the repository's name.
  base: '/MediaCopy3000/',
  // A link to a page that does not exist fails the build. The manual already makes that bargain
  // for facts, through `just xref`; this makes it for links.
  ignoreDeadLinks: false,
  lastUpdated: true,
  // The sitemap tells a search engine that the two trees are one site.
  sitemap: { hostname: 'https://tchoutri.github.io/MediaCopy3000/' },

  rewrites: {
    'en/:rest*': ':rest*',
  },

  locales: {
    root: { label: 'English', lang: 'en-US', dir: 'ltr', ...en },
    fr: { label: 'Français', lang: 'fr-FR', dir: 'ltr', ...fr },
  },

  themeConfig: {
    socialLinks: [
      { icon: 'github', link: 'https://github.com/tchoutri/MediaCopy3000' },
    ],

    // The local search index is built per locale. A page goes to the index of the folder it is in,
    // and `en/` is not a locale key, so the English pages go to the root index. That is the index
    // the reader asks for at `/`.
    search: {
      provider: 'local',
      options: {
        detailedView: true,
        locales: {
          fr: {
            translations: {
              button: {
                buttonText: 'Rechercher',
                buttonAriaLabel: 'Rechercher',
              },
              modal: {
                displayDetails: 'Afficher la liste détaillée',
                resetButtonTitle: 'Réinitialiser la recherche',
                backButtonTitle: 'Fermer la recherche',
                noResultsText: 'Aucun résultat pour',
                footer: {
                  selectText: 'pour sélectionner',
                  selectKeyAriaLabel: 'entrée',
                  navigateText: 'pour naviguer',
                  navigateUpKeyAriaLabel: 'flèche haut',
                  navigateDownKeyAriaLabel: 'flèche bas',
                  closeText: 'pour fermer',
                  closeKeyAriaLabel: 'échap',
                },
              },
            },
          },
        },
      },
    },
  },
})
