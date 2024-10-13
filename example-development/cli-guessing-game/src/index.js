import * as Node from "../../../runner-compiled/node.js"

const elmApp = Node.compileElm(import.meta.dirname, "Main.elm").init()
Node.programStart({ ports: elmApp.ports })
