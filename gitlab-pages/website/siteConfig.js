const repoUrl = 'https://gitlab.com/ligolang/ligo';

let reasonHighlightJs = require('reason-highlightjs');

const siteConfig = {
  title: 'LIGO', // Title for your website.
  tagline: 'LIGO is a friendly smart-contract language for Tezos',
  taglineSub: 'Michelson was never so easy',
  url: 'https://ligolang.org', // Your website URL
  baseUrl: '/', // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: 'ligo',
  organizationName: 'marigold',
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    { href: 'https://ide.ligolang.org/', label: 'Try Online' },
    { doc: 'intro/installation', label: 'Install' },
    { doc: 'intro/what-and-why', label: 'Docs' },
    {
      doc: 'tutorials/get-started/tezos-taco-shop-smart-contract',
      label: 'Tutorials'
    },
    { blog: true, label: 'Blog' },
    // TODO: { href: "/odoc", label: "Api" },
    // { doc: 'contributors/origin', label: 'Contribute' },
    { href: '/contact', label: 'Ask Questions' },
    { search: true }
  ],

  footerLinks: {
    docs: [
      { doc: 'intro/installation', label: 'Install' },
      { doc: 'api/cli-commands', label: 'CLI Commands' },
      { doc: 'contributors/origin', label: 'Contribute' },
      { href: '/odoc', label: 'Api Documentation' }
    ],
    community: [
      {
        href: 'https://tezos.stackexchange.com/questions/tagged/ligo',
        label: 'Tezos Stack Exchange',
        blankTarget: true
      },
      {
        href: 'https://discord.gg/9rhYaEt',
        label: 'Discord',
        blankTarget: true
      }
    ],
    more: [
      {
        doc: 'tutorials/get-started/tezos-taco-shop-smart-contract',
        label: 'Tutorials'
      },
      { href: repoUrl, label: 'Gitlab' }
    ]
  },

  /* path to images for header/footer */
  footerIcon: 'img/logo.svg',
  favicon: 'img/logo.svg',

  /* Colors for website */
  colors: {
    primaryColor: '#1A1A1A',
    secondaryColor: '#1A1A1A'
  },

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `© ${new Date().getFullYear()} LIGO. All rights reserved.`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: 'default',
    hljs: function (hljs) {
      hljs.registerLanguage('reasonligo', reasonHighlightJs);
      hljs.registerLanguage('pascaligo', function (hljs) {
        return {
          // case_insensitive: true,
          beginKeywords: '',
          keywords: {
            keyword:
              'and begin block case const contains down else end fail for ' +
              'from function if in is list map mod nil not of or patch ' +
              'procedure record remove set skip step then to type var while with',
            literal: 'true false unit int string some none bool nat list'
          },
          lexemes: '[a-zA-Z][a-zA-Z0-9_]*',
          contains: [
            hljs.C_LINE_COMMENT_MODE,

            {
              className: 'type',
              begin: /[A-Z][a-z]+/
            },
            {
              begin: /[*+-:;\(\)\{\}|\>\<]/
              // className: 'ignore'
            }
          ]
        };
      });
    }
  },

  // Add custom scripts here that would be placed in <script> tags.
  scripts: ['https://buttons.github.io/buttons.js'],

  // On page navigation for the current documentation page.
  onPageNav: 'separate',
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  ogImage: 'img/logo.svg',
  twitterImage: 'img/undraw_tweetstorm.svg',

  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  // enableUpdateTime: true,

  // You may provide arbitrary config keys to be used as needed by your
  // template. For example, if you need your repo's URL...
  repoUrl: repoUrl,
  stylesheets: [
    'https://fonts.googleapis.com/css?family=DM+Sans:400,400i,500,500i,700,700i|Open+Sans:300,300i,400,600|Source+Code+Pro&display=swap'
  ],
  algolia: {
    apiKey: '12be98d9fd4242a5f16b70a5cc6b0158',
    indexName: 'ligolang',
    algoliaOptions: {} // Optional, if provided by Algolia
  },
  gaTrackingId: 'UA-153751765-1',
  gaGtag: true
};

module.exports = siteConfig;
