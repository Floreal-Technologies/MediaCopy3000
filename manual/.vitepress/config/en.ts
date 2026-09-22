import type { DefaultTheme, UserConfig } from "vitepress"

export default {
  description: "Verified media offload and ASC MHL verification for videographers",

  themeConfig: {
    outline: [2, 3],

    sidebar: [
      { text: "User Guide", base: "/", items: sidebarUserGuide() },
      { text: "Reference", base: "/", items: sidebarReference() },
    ],

    editLink: {
      pattern: "https://github.com/Floreal-Technologies/MediaCopy3000/edit/main/manual/:path",
      text: "Edit this page on GitHub",
    },

    lastUpdated: {
      text: "Last updated",
      formatOptions: { dateStyle: "medium", timeStyle: "short" },
    },
  },
} satisfies Pick<UserConfig<DefaultTheme.Config>, "description" | "themeConfig">

function sidebarUserGuide(): DefaultTheme.SidebarItem[] {
  return [
    { text: "Introduction", link: "introduction" },
    { text: "Installation", link: "installation" },
    { text: "The window", link: "the-window" },
    { text: "Offload a media source", link: "offload" },
    { text: "The plan", link: "the-plan" },
    { text: "Seal the media source first", link: "seal-first" },
    { text: "While a job runs", link: "running-a-job" },
    { text: "Verify a folder", link: "verify" },
    { text: "Seal a media source", link: "seal" },
    { text: "History and generations", link: "history" },
    { text: "Reports", link: "reports" },
    { text: "Findings", link: "findings" },
    { text: "Event log", link: "event-log"},
    { text: "The command line", link: "command-line"},
    { text: "Preferences", items: [{ text: "Appearance", link: "appearance" }] },
    { text: "Keyboard Shortcuts", link: "keyboard" },
  ]
}

function sidebarReference(): DefaultTheme.SidebarItem[] {
  return [
    { text: "The ASC MHL format", link: "mhl-format" },
    { text: "Limits", link: "limits" },
  ]
}

