/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

// List of projects/orgs using your project for the users page.
const partners = [
  {
    caption: "Nomadic Labs",
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: "/img/nomadic-logo.svg",
    infoLink: "https://www.nomadic-labs.com/",
    pinned: true
  },
  {
    caption: "Tocqueville Group",
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: "/img/tq-logo-2.svg",
    infoLink: "https://tqgroup.io/",
    pinned: true
  },
  {
    caption: "Stove Labs",
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: "/img/stove-logo.svg",
    infoLink: "https://stove-labs.com",
    pinned: true
  }
];

const team = [
  {
    caption: "Gabriel Alfour",
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: "/img/user.svg",
    infoLink: "https://gitlab.com/gabriel.alfour",
    pinned: true
  },
  {
    caption: "Georges Dupéron",
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: "/img/user.svg",
    infoLink: "https://gitlab.com/georges.duperon",
    pinned: true
  },
  {
    caption: "Christian Rinderknecht",
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: "/img/christian.jpeg",
    infoLink: "https://github.com/rinderknecht",
    pinned: true
  },
  {
    caption: "Brice Aldrich",
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: "/img/brice.png",
    infoLink: "https://github.com/DefinitelyNotAGoat",
    pinned: true
  },
  {
    caption: "Matej Sima",
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: "/img/matej.jpg",
    infoLink: "https://github.com/maht0rz",
    pinned: true
  }
];

const siteConfig = {
  title: "LIGO", // Title for your website.
  tagline:
    "LIGO is a friendly smart-contract language for Tezos",
  taglineSub: "Michelson was never so easy",
  url: "https://ligolang.org", // Your website URL
  baseUrl: "/", // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  // Used for publishing and more
  projectName: "ligo",
  organizationName: "marigold",
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    { doc: "intro/what-and-why", label: "Docs" },
    {
      doc: "tutorials/get-started/tezos-taco-shop-smart-contract",
      label: "Tutorials"
    },
    { blog: true, label: "Blog" },
    // TODO: { href: "/odoc", label: "Api" },
    { doc: "contributors/origin", label: "Contribute" },
    { href: "https://discord.gg/9rhYaEt", label: "" },
    { search: true }
  ],

  // If you have users set above, you add it here:
  partners,
  team,

  /* path to images for header/footer */
  footerIcon: "img/logo.svg",
  favicon: "img/logo.svg",

  /* Colors for website */
  colors: {
    primaryColor: "#1A1A1A",
    secondaryColor: "#1A1A1A"
  },

  /* Custom fonts for website */
  /*
  fonts: {
    myFont: [
      "Times New Roman",
      "Serif"
    ],
    myOtherFont: [
      "-apple-system",
      "system-ui"
    ]
  },
  */

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `© ${new Date().getFullYear()} LIGO. All rights reserved.`,

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks.
    theme: "default",
    hljs: function (hljs) {
      hljs.registerLanguage('pascaligo', function (hljs) {
        return {
          // case_insensitive: true,
          beginKeywords: '',
          keywords: {
            keyword: 'and begin block case const contains down else end fail for ' +
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
              begin: /[*+-:;\(\)\{\}|\>\<]/,
              // className: 'ignore'
            }
          ]
        }
      });
    }
  },

  // Add custom scripts here that would be placed in <script> tags.
  scripts: ["https://buttons.github.io/buttons.js"],

  // On page navigation for the current documentation page.
  onPageNav: "separate",
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  ogImage: "img/logo.svg",
  twitterImage: "img/undraw_tweetstorm.svg",

  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  // enableUpdateTime: true,

  // You may provide arbitrary config keys to be used as needed by your
  // template. For example, if you need your repo's URL...
  repoUrl: "https://gitlab.com/ligolang/ligo",
  stylesheets: [
    "https://fonts.googleapis.com/css?family=DM+Sans|Open+Sans|Source+Code+Pro&display=swap"
  ],
  algolia: {
    apiKey: "12be98d9fd4242a5f16b70a5cc6b0158",
    indexName: "ligolang",
    algoliaOptions: {} // Optional, if provided by Algolia
  }
};

module.exports = siteConfig;
