#!/usr/bin/env node

import * as path from "node:path"
import * as fs from "node:fs"

// inspired by https://github.com/jfmengels/node-elm-review/blob/2651aa4cc53726bbf9107a42f0756b849db3e1e7/new-package/maintenance/update-examples-from-preview.js

fs.readdir(
    path.resolve(import.meta.dirname),
    (_errors, files) => files.forEach(sub => {
        if (sub !== "README.md" && sub !== "update-example.js") {
            copyExampleDevelopmentToExample(sub)
        }
    })
)

function copyExampleDevelopmentToExample(sub) {
    const packageElmJson = JSON.parse(fs.readFileSync(path.resolve(import.meta.dirname, "..", "elm.json")))
    const packageSrcPath = path.resolve(import.meta.dirname, "..", "src")
    const exampleElmJsonPath = path.resolve(import.meta.dirname, "..", "example", sub, "elm.json")
    const indexHtmlPath = path.resolve(import.meta.dirname, sub, "index.html")
    const target =
        fs.existsSync(indexHtmlPath) ?
            "web"
            :
            "node"

    fs.cpSync(
        path.resolve(import.meta.dirname, sub, "src"),
        path.resolve(import.meta.dirname, "..", "example", sub, "src"),
        { recursive: true, filter: (source, _) => !source.endsWith(".js") }
    )
    if (fs.existsSync(path.resolve(import.meta.dirname, sub, "public"))) {
        fs.cpSync(
            path.resolve(import.meta.dirname, sub, "public"),
            path.resolve(import.meta.dirname, "..", "example", sub, "public"),
            { recursive: true }
        )
    }
    fs.cpSync(
        path.resolve(import.meta.dirname, sub, "elm.json"),
        exampleElmJsonPath
    )

    const exampleElmJson = JSON.parse(fs.readFileSync(exampleElmJsonPath))

    // Remove the source directory pointing to the package's src/
    exampleElmJson['source-directories'] = exampleElmJson['source-directories'].filter(
        (sourceDirectory) =>
            path.resolve(import.meta.dirname, "..", "example", sub, sourceDirectory) !== packageSrcPath
    )
    exampleElmJson.dependencies.direct[packageElmJson.name] = packageElmJson.version
    moveFromDirectToIndirect(exampleElmJson, "miniBill/elm-fast-dict")
    moveFromDirectToIndirect(exampleElmJson, "miniBill/elm-rope")
    moveFromDirectToIndirect(exampleElmJson, "elm/bytes")
    fs.writeFileSync(exampleElmJsonPath, JSON.stringify(exampleElmJson, null, 4))

    if (target === "web") {
        fs.cpSync(
            path.resolve(import.meta.dirname, sub, "vite.config.js"),
            path.resolve(import.meta.dirname, "..", "example", sub, "vite.config.js")
        )
        fs.cpSync(
            indexHtmlPath,
            path.resolve(import.meta.dirname, "..", "example", sub, "index.html")
        )
        fs.writeFileSync(
            path.resolve(import.meta.dirname, "..", "example", sub, "package.json"),
            `{
    "type": "module",
    "main": "src/index.js",
    "dependencies": {
        "@lue-bird/elm-state-interface-experimental": "^2.0.2",
        "vite": "^5.1.2",
        "vite-plugin-elm-watch": "^1.3.2"
    }
}`
        )
        fs.writeFileSync(
            path.resolve(import.meta.dirname, "..", "example", sub, "src", "index.js"),
            `import * as Web from "@lue-bird/elm-state-interface-experimental/web"
import Main from "./Main.elm"

const elmApp = Main.init()
Web.programStart({ ports: elmApp.ports, domElement: document.getElementById("app") })
`
        )
    } else if (target === "node") {
        fs.writeFileSync(
            path.resolve(import.meta.dirname, "..", "example", sub, "package.json"),
            `{
    "type": "module",
    "main": "src/index.js",
    "dependencies": {
        "@lue-bird/elm-state-interface-experimental": "^2.0.2"
    }
}`
        )
        fs.writeFileSync(
            path.resolve(import.meta.dirname, "..", "example", sub, "src", "index.js"),
            `import * as Node from "@lue-bird/elm-state-interface-experimental/node"

const elmApp = Node.compileElm(import.meta.dirname, "Main.elm").init()
Node.programStart({ ports: elmApp.ports })
`
        )
    }
}
function moveFromDirectToIndirect(elmJson, dependencyName) {
    elmJson.dependencies.indirect[dependencyName] =
        elmJson.dependencies.direct[dependencyName]
    delete elmJson.dependencies.direct[dependencyName]
}