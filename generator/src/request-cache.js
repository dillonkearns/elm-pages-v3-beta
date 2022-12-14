const path = require("path");
const kleur = require("kleur");
const fsPromises = require("fs/promises");

const defaultHttpCachePath = "./.elm-pages/http-cache";

/** @typedef {{kind: 'cache-response-path', value: string} | {kind: 'response-json', value: JSON}} Response */

/**
 * @param {string} mode
 * @param {{url: string;headers: {[x: string]: string;};method: string;body: Body;}} rawRequest
 * @param {string} portsFile
 * @param {boolean} hasFsAccess
 * @returns {Promise<Response>}
 */
function lookupOrPerform(portsFile, mode, rawRequest, hasFsAccess, useCache) {
  const fetch = require("make-fetch-happen").defaults({
    cache: mode === "build" ? "no-cache" : "default",
  });
  return new Promise(async (resolve, reject) => {
    const request = toRequest(rawRequest);

    let portBackendTask = {};
    let portBackendTaskImportError = null;
    try {
      if (portsFile === undefined) {
        throw "missing";
      }
      const portBackendTaskPath = path.resolve(portsFile);
      // On Windows, we need cannot use paths directly and instead must use a file:// URL.
      // portBackendTask = await require(url.pathToFileURL(portBackendTaskPath).href);
      portBackendTask = require(portBackendTaskPath);
    } catch (e) {
      portBackendTaskImportError = e;
    }

    if (request.url === "elm-pages-internal://port") {
      try {
        const { input, portName } = rawRequest.body.args[0];

        if (!portBackendTask[portName]) {
          if (portBackendTaskImportError === null) {
            resolve({
              kind: "response-json",
              value: jsonResponse({
                "elm-pages-internal-error": "PortNotDefined",
              }),
            });
          } else if (portBackendTaskImportError === "missing") {
            resolve({
              kind: "response-json",
              value: jsonResponse({
                "elm-pages-internal-error": "MissingPortsFile",
              }),
            });
          } else {
            resolve({
              kind: "response-json",
              value: jsonResponse({
                "elm-pages-internal-error": "ErrorInPortsFile",
                error:
                  (portBackendTaskImportError &&
                    portBackendTaskImportError.stack) ||
                  "",
              }),
            });
          }
        } else if (typeof portBackendTask[portName] !== "function") {
          resolve({
            kind: "response-json",
            value: jsonResponse({
              "elm-pages-internal-error": "ExportIsNotFunction",
              error: typeof portBackendTask[portName],
            }),
          });
        } else {
          try {
            resolve({
              kind: "response-json",
              value: jsonResponse(await portBackendTask[portName](input)),
            });
          } catch (portCallError) {
            resolve({
              kind: "response-json",
              value: jsonResponse({
                "elm-pages-internal-error": "PortCallError",
                error: portCallError,
              }),
            });
          }
        }
      } catch (error) {
        console.trace(error);
        reject({
          title: "BackendTask.Port Error",
          message: error.toString(),
        });
      }
    } else {
      try {
        console.time(`fetch ${request.url}`);
        const response = await safeFetch(fetch, request.url, {
          method: request.method,
          body: request.body,
          headers: {
            "User-Agent": "request",
            ...request.headers,
          },
          ...rawRequest.cacheOptions,
        });

        console.timeEnd(`fetch ${request.url}`);
        const expectString = request.headers["elm-pages-internal"];

        let body;
        let bodyKind;
        if (expectString === "ExpectJson") {
          try {
            body = await response.buffer();
            body = JSON.parse(body.toString("utf-8"));
            bodyKind = "json";
          } catch (error) {
            body = body.toString("utf8");
            bodyKind = "string";
          }
        } else if (
          expectString === "ExpectBytes" ||
          expectString === "ExpectBytesResponse"
        ) {
          body = await response.buffer();
          try {
            body = body.toString("base64");
            bodyKind = "bytes";
          } catch (e) {
            body = body.toString("utf8");
            bodyKind = "string";
          }
        } else if (expectString === "ExpectWhatever") {
          bodyKind = "whatever";
          body = null;
        } else if (
          expectString === "ExpectResponse" ||
          expectString === "ExpectString"
        ) {
          bodyKind = "string";
          body = await response.text();
        } else {
          throw `Unexpected expectString ${expectString}`;
        }

        resolve({
          kind: "response-json",
          value: {
            headers: Object.fromEntries(response.headers.entries()),
            statusCode: response.status,
            body,
            bodyKind,
            url: response.url,
            statusText: response.statusText,
          },
        });
      } catch (error) {
        console.trace("@@@ request-cache2 HTTP error", error);
        reject({
          title: "BackendTask.Http Error",
          message: `${kleur.yellow().underline(request.url)} ${error.toString()}
`,
        });
      }
    }
  });
}

/**
 * @param {{url: string; headers: {[x: string]: string}; method: string; body: Body } } elmRequest
 */
function toRequest(elmRequest) {
  const elmHeaders = Object.fromEntries(elmRequest.headers);
  let contentType = toContentType(elmRequest.body);
  let headers = { ...contentType, ...elmHeaders };
  return {
    url: elmRequest.url,
    method: elmRequest.method,
    headers,
    body: toBody(elmRequest.body),
  };
}
/**
 * @param {Body} body
 */
function toBody(body) {
  switch (body.tag) {
    case "EmptyBody": {
      return null;
    }
    case "StringBody": {
      return body.args[1];
    }
    case "BytesBody": {
      return Buffer.from(body.args[1], "base64");
    }
    case "JsonBody": {
      return JSON.stringify(body.args[0]);
    }
  }
}

/**
 * @param {Body} body
 * @returns Object
 */
function toContentType(body) {
  switch (body.tag) {
    case "EmptyBody": {
      return {};
    }
    case "StringBody": {
      return { "Content-Type": body.args[0] };
    }
    case "BytesBody": {
      return { "Content-Type": body.args[0] };
    }
    case "JsonBody": {
      return { "Content-Type": "application/json" };
    }
  }
}

/** @typedef { { tag: 'EmptyBody'} |{ tag: 'BytesBody'; args: [string, string] } |  { tag: 'StringBody'; args: [string, string] } | {tag: 'JsonBody'; args: [ Object ] } } Body  */
function requireUncached(mode, filePath) {
  if (mode === "dev-server") {
    // for the build command, we can skip clearing the cache because it won't change while the build is running
    // in the dev server, we want to clear the cache to get a the latest code each time it runs
    delete require.cache[require.resolve(filePath)];
  }
  return require(filePath);
}

/**
 * @param {unknown} json
 */
function jsonResponse(json) {
  return { bodyKind: "json", body: json };
}

async function safeFetch(fetch, url, options) {
  const { cachePath, ...optionsWithoutCachePath } = options;
  const cachePathWithDefault = cachePath || defaultHttpCachePath;
  if (await canAccess(cachePathWithDefault)) {
    return await fetch(url, options);
  } else {
    return await fetch(url, { cache: "no-store", ...optionsWithoutCachePath });
  }
}

async function canAccess(filePath) {
  try {
    await fsPromises.access(
      filePath,
      fsPromises.constants.R_OK | fsPromises.constants.W_OK
    );
    return true;
  } catch {
    return false;
  }
}

module.exports = { lookupOrPerform };
