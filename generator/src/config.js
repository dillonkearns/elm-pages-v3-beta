const path = require("path");

async function resolveConfig() {
  const initialConfig = await await import(
    path.join(process.cwd(), "elm-pages.config.mjs")
  )
    .then(async (elmPagesConfig) => {
      return (
        elmPagesConfig.default || {
          headTagsTemplate: defaultHeadTagsTemplate,
        }
      );
    })
    .catch((error) => {
      if (error.code && error.code === 'ERR_MODULE_NOT_FOUND') {
        console.warn(
          "No `elm-pages.config.mjs` file found. Using default config."
        );
        return {
          headTagsTemplate: defaultHeadTagsTemplate,
          vite: {},
        };
      } else {
        console.error("There was an error running `elm-pages.config.mjs`:");
        console.error(error);
      }
    });

  return {
    preloadTagForFile: function () {
      return true;
    },
    headTagsTemplate: defaultHeadTagsTemplate,
    vite: {},
    ...initialConfig,
  };
}

function defaultHeadTagsTemplate(context) {
  return `
<link rel="stylesheet" href="/style.css" />
<meta name="generator" content="elm-pages v${context.cliVersion}" />
`;
}

module.exports = { resolveConfig };
