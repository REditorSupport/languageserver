import { defineConfig } from 'vitepress'

const repo = 'https://github.com/REditorSupport/languageserver'

export default defineConfig({
  lang: 'en-US',
  title: 'R languageserver',
  description: 'A guide to writing, navigating, and refactoring R in your editor with the R language server.',
  base: '/languageserver/',
  cleanUrls: true,
  lastUpdated: true,
  themeConfig: {
    nav: [
      { text: 'Get started', link: '/guide/quick-start' },
      { text: 'Features', link: '/guide/writing-r' },
      { text: 'Reference', link: '/reference/settings' }
    ],
    sidebar: [
      {
        text: 'Get started',
        items: [
          { text: 'Five-minute walkthrough', link: '/guide/quick-start' },
          { text: 'Set up your editor', link: '/guide/editors' }
        ]
      },
      {
        text: 'Feature guides',
        items: [
          { text: 'Write and understand R', link: '/guide/writing-r' },
          { text: 'Navigate and refactor', link: '/guide/navigation' },
          { text: 'Diagnose and format code', link: '/guide/diagnostics-formatting' },
          { text: 'R Markdown and Quarto', link: '/guide/literate' },
          { text: 'Development features', link: '/guide/development' }
        ]
      },
      {
        text: 'Help and reference',
        items: [
          { text: 'Troubleshooting', link: '/guide/troubleshooting' },
          { text: 'Settings', link: '/reference/settings' },
          { text: 'Editor capabilities', link: '/reference/capabilities' },
          { text: 'Launch the server', link: '/reference/run' },
          { text: 'Custom backends and styles', link: '/reference/adapters' }
        ]
      }
    ],
    search: { provider: 'local' },
    outline: { level: [2, 3] },
    socialLinks: [{ icon: 'github', link: repo }],
    editLink: {
      pattern: repo + '/edit/master/website/docs/:path',
      text: 'Edit this page on GitHub'
    }
  }
})
