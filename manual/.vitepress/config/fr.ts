import type { DefaultTheme, UserConfig } from "vitepress"

export default {
  description: "Déchargement de médias vérifié et vérification ASC MHL pour les vidéastes",

  themeConfig: {
    outline: { level: [2, 3], label: "Sur cette page" },

    sidebar: [
      { text: "Guide de l'utilisateur", base: "/fr/", items: sidebarGuideUtilisateur() },
      { text: "Référence", base: "/fr/", items: sidebarReference() },
    ],

    editLink: {
      pattern: "https://github.com/Floreal-Technologies/MediaCopy3000/edit/main/manual/:path",
      text: "Modifier cette page sur GitHub",
    },

    lastUpdated: {
      text: "Dernière mise à jour",
      formatOptions: { dateStyle: "medium", timeStyle: "short" },
    },

    docFooter: { prev: "Page précédente", next: "Page suivante" },

    notFound: {
      title: "PAGE INTROUVABLE",
      quote: "Cette page n'existe pas. Elle n'a peut-être jamais existé.",
      linkLabel: "aller à la page d'accueil",
      linkText: "Retour à l'accueil",
    },

    langMenuLabel: "Changer de langue",
    returnToTopLabel: "Retour en haut",
    sidebarMenuLabel: "Menu",
    darkModeSwitchLabel: "Apparence",
    lightModeSwitchTitle: "Passer au thème clair",
    darkModeSwitchTitle: "Passer au thème sombre",
    skipToContentLabel: "Aller au contenu",
  },
} satisfies Pick<UserConfig<DefaultTheme.Config>, "description" | "themeConfig">

function sidebarGuideUtilisateur(): DefaultTheme.SidebarItem[] {
  return [
    { text: "Introduction", link: "introduction" },
    { text: "Installation", link: "installation" },
    { text: "La fenêtre principale", link: "the-window" },
    { text: "Décharger des médias", link: "offload" },
    { text: "Le plan", link: "the-plan" },
    { text: "Sceller d'abord la source de médias", link: "seal-first" },
    { text: "Exécution d'une tâche", link: "running-a-job" },
    { text: "Vérifier un dossier", link: "verify" },
    { text: "Sceller une source de médias", link: "seal" },
    { text: "Historique et générations", link: "history" },
    { text: "Rapports", link: "reports" },
    { text: "Constats", link: "findings" },
    { text: "Journal des événements", link: "event-log" },
    { text: "The command line", link: "command-line"},
    { text: "Préférences", items: [{ text: "Apparence", link: "appearance" }] },
    { text: "Raccourcis clavier", link: "keyboard" },
  ]
}

function sidebarReference(): DefaultTheme.SidebarItem[] {
  return [
    { text: "Le format ASC MHL", link: "mhl-format" },
    { text: "Limites", link: "limits" },
  ]
}

