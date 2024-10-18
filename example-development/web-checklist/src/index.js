import * as Web from "../../../runner-compiled/web.js"
import Main from "./Main.elm"

const elmApp = Main.init()
Web.programStart({ ports: elmApp.ports, domElement: document.getElementById("app") })
