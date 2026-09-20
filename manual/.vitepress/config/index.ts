import { defineConfig } from 'vitepress'
import en from './en'
import fr from './fr'

export default defineConfig({
  title: 'MediaCopy 3000',
  description: 'Verified media offload and ASC MHL verification for videographers',
  base: '/',
  ignoreDeadLinks: false,
  lastUpdated: true,
  sitemap: { hostname: 'https://docs.floreal.tech/MediaCopy3000/' },

  rewrites: {
    'en/:rest*': ':rest*',
  },

  locales: {
    root: { label: 'English', lang: 'en-US', dir: 'ltr', ...en },
    fr: { label: 'Français', lang: 'fr-FR', dir: 'ltr', ...fr },
  },

  themeConfig: {
    socialLinks: [
      { icon: 'github', link: 'https://github.com/Floreal-Technologies/MediaCopy3000' },
    ],

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
