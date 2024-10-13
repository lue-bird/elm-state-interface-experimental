import * as fs from "node:fs"
import * as path from "node:path"
import * as child_process from "node:child_process"

export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void,
        unsubscribe: (callback: (fromElm: any) => void) => void
    }
    fromJs: { send: (toElm: any) => void }
}

/** Small helper for creating a runnable elm program. Requires the `elm` binary to be available.
 * Feel free to use a custom loader that e.g. allows debug or uses an alternative compiler.
 * 
 * @param elmProjectDirectoryPath The path to the elm application. Use `import.meta.dirname`
 * @param mainElmModuleNamePath usually Main.elm or src/Main.elm depending on where your index.js is
 * @returns An elm worker program you can start whenever you want which then provides its ports
 */
export function compileElm(elmProjectDirectoryPath: string, mainElmModuleNamePath: string): { init: () => { ports: ElmPorts } } {
    if (elmProjectDirectoryPath === undefined) {
        console.error("To access import.meta.dirname you need at least Node.js 20.11 / 21.2: https://stackoverflow.com/a/50052194")
        throw new Error()
    }
    const elmJsPath = path.join(elmProjectDirectoryPath, "elm-stuff", "temporary-elm.js")

    child_process.execSync(
        // if you want to use Debug.log etc while prototyping or debugging, remove the --optimize flag
        // if you need extra performance switch to https://github.com/mdgriffith/elm-optimize-level-2
        `elm make ${mainElmModuleNamePath} --output "${elmJsPath}" --optimize`,
        { cwd: elmProjectDirectoryPath }
    )
    eval(
        fs.readFileSync(elmJsPath, { encoding: "utf8" })
            .replace("(this)", "(globalThis)")
    )
    fs.unlinkSync(elmJsPath)
    return (globalThis as any).Elm.Main
}



export function programStart(appConfig: { ports: ElmPorts }) {
    function listenToElm(fromElm: { id: string, diff: { tag: "Add" | "Edit" | "Remove", value: any } }) {
        // console.log("elm → js: ", fromElm)
        function sendToElm(eventData: void) {
            const toElm = {
                id: fromElm.id,
                eventData: eventData
            }
            appConfig.ports.fromJs.send(toElm)
            // console.log("js → elm: ", toElm)
        }
        interfaceDiffImplementation(fromElm.diff.tag, sendToElm, fromElm.id)(fromElm.diff.value)
    }
    appConfig.ports.toJs.subscribe(listenToElm)
    function interfaceDiffImplementation(tag: "Add" | "Edit" | "Remove", sendToElm: (v: any) => void, id: string): ((config: any) => void) {
        switch (tag) {
            case "Add": return (config: { tag: string, value: any }) => {
                const abortController = new AbortController()
                abortControllers.set(id, abortController)
                interfaceAddImplementation(config.tag, sendToElm, abortController.signal)(config.value)
            }
            case "Edit": return (config: { tag: string, value: any }) => {
                interfaceEditImplementation(id)(config.value)
            }
            case "Remove": return (_config: null) => {
                const abortController = abortControllers.get(id)
                if (abortController !== undefined) {
                    abortController.abort()
                    abortControllers.delete(id)
                } else {
                    warn("bug: trying to remove an interface that was already aborted")
                }
            }
        }
    }
    function interfaceAddImplementation(tag: string, sendToElm: (v: any) => void, abortSignal: AbortSignal): ((config: any) => void) {
        switch (tag) {
            case "StandardInListen": return (_config: null) => {
                function listen(buffer: Buffer) {
                    sendToElm(buffer.toString())
                }
                process.stdin.addListener("data", listen)
                abortSignal.addEventListener("abort", (_event) => {
                    process.stdin.removeListener("data", listen);
                })
            }
            case "StandardOutWrite": return (text: string) => {
                process.stdout.write(text)
            }
            case "StandardErrWrite": return (text: string) => {
                process.stderr.write(text)
            }
            case "ConsoleLog": return (message: string) => {
                console.log(message)
            }
            case "ConsoleWarn": return (message: string) => {
                console.warn(message)
            }
            case "ConsoleError": return (message: string) => {
                console.error(message)
            }
            case "ConsoleError": return (_config: null) => {
                console.clear()
            }
            case "WorkingDirectoryPathRequest": return (_config: null) => {
                sendToElm(process.cwd())
            }
            case "LaunchArgumentsRequest": return (_config: null) => {
                sendToElm(process.argv)
            }
            case "Exit": return (code: number) => {
                appConfig.ports.toJs.unsubscribe(listenToElm)
                abortControllers.clear() // disable any abort signal listeners
                process.stdin.unref()
                process.exitCode = code
            }
            case "ProcessTitleSet": return (newTitle: string) => {
                process.title = newTitle
            }
            case "TerminalSizeRequest": return (_config: null) => {
                sendToElm({ lines: process.stdout.rows, columns: process.stdout.columns })
            }
            case "TerminalSizeChangeListen": return (_config: null) => {
                function onResize(_event: any): void {
                    sendToElm({ lines: process.stdout.rows, columns: process.stdout.columns })
                }
                process.stdout.addListener("resize", onResize)
                abortSignal.addEventListener("abort", (_event) => {
                    process.stdout.removeListener("resize", onResize)
                })
            }
            case "HttpRequest": return (config: HttpRequest) => {
                httpFetch(config, abortSignal).then(sendToElm)
            }
            case "TimePosixRequest": return (_config: null) => {
                sendToElm(Date.now())
            }
            case "TimezoneOffsetRequest": return (_config: null) => {
                // Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L38-L52
                sendToElm(new Date().getTimezoneOffset())
            }
            case "TimezoneNameRequest": return (_config: null) => {
                sendToElm(Intl.DateTimeFormat().resolvedOptions().timeZone)
            }
            case "TimePeriodicallyListen": return (config: { milliSeconds: number }) => {
                const timePeriodicallyListenId =
                    setInterval(
                        () => { sendToElm(Date.now()) },
                        config.milliSeconds
                    )
                abortSignal.addEventListener("abort", _event => {
                    clearInterval(timePeriodicallyListenId)
                })
            }
            case "TimeOnce": return (config: { pointInTime: number }) => {
                const timeOnceId =
                    setTimeout(
                        () => { sendToElm(Date.now()) },
                        config.pointInTime - Date.now()
                    )
                abortSignal.addEventListener("abort", _event => {
                    clearInterval(timeOnceId)
                })
            }
            case "RandomUnsignedInt32sRequest": return (config: number) => {
                sendToElm(Array.from(crypto.getRandomValues(new Uint32Array(config))))
            }
            case "DirectoryMake": return (write: { path: string }) => {
                fs.promises.mkdir(write.path, { recursive: true })
                    .then(() => { })
                    .catch((err) => warn("failed to make directory " + err))
            }
            case "FileRemove": return (path: string) => {
                fs.promises.unlink(path)
                    .then(() => { })
                    .catch((err) => warn("failed to unlink file " + err))
            }
            case "FileUtf8Write": return (write: { content: string, path: string }) => {
                fileUtf8Write(write, abortSignal)
            }
            case "FileUtf8Request": return (path: string) => {
                fs.promises.readFile(
                    path,
                    { encoding: "utf-8", signal: abortSignal }
                )
                    .then((content) => { sendToElm(content) })
                    .catch((err) => warn("failed to read file " + err))
            }
            case "FileInfoRequest": return (path: string) => {
                fs.promises.stat(path)
                    .then((stats) => {
                        sendToElm({
                            byteCount: stats.size,
                            kind: stats.isDirectory() ? "Directory" : "File",
                            lastContentChangePosixMilliseconds: stats.mtimeMs
                        })
                    })
                    .catch((_enoend) => {
                        sendToElm(null)
                    })
            }
            case "FileChangeListen": return (path: string) => {
                watchPath(path, abortSignal, event => sendToElm(event))
            }
            default: return (_config: any) => {
                notifyOfUnknownMessageKind("Add." + tag)
            }
        }
    }
    function interfaceEditImplementation(tag: string): ((config: any) => void) {
        switch (tag) {
            default: return (_config: any) => {
                notifyOfUnknownMessageKind("Edit." + tag)
            }
        }
    }
}

const abortControllers: Map<string, AbortController> = new Map()




function fileUtf8Write(write: { path: string, content: string }, abortSignal: AbortSignal | undefined) {
    fs.promises.writeFile(
        write.path,
        write.content,
        { encoding: "utf-8", signal: abortSignal }
    )
        .then(() => { })
        .catch((err) => warn("failed to write to file " + err))
}

function watchPath(
    pathToWatch: string,
    abortSignal: AbortSignal,
    on: (event: { tag: "Removed" | "AddedOrChanged", value: string }) => void
) {
    // most editors chunk up their file edits in 2, see
    // https://stackoverflow.com/questions/12978924/fs-watch-fired-twice-when-i-change-the-watched-file
    let debounced = true

    fs.watch(
        pathToWatch,
        { recursive: true, signal: abortSignal },
        (_event, fileName) => {
            if (debounced) {
                debounced = false
                if (fileName !== null) {
                    const fullPath = path.join(pathToWatch, fileName)
                    if (fs.existsSync(fullPath)) {
                        on({ tag: "AddedOrChanged", value: fullPath })
                    } else {
                        on({ tag: "Removed", value: fullPath })
                    }
                }
            } else {
                setTimeout(() => { debounced = true }, 100)
            }
        }
    )
}



interface HttpRequest {
    url: string
    method: string
    headers: { name: string, value: string }[]
    expect: Expect
    body: HttpRequestBody
}
type Expect = | "String" | "Bytes" | "Whatever"
type HttpRequestBody =
    | { tag: "Uint8Array", value: Uint8Array }
    | { tag: "String", value: string }
    | { tag: "Empty", value: null }

type HttpResponse =
    | { tag: "Success", value: ResponseSuccess }
    | { tag: "Error", value: any }
interface ResponseSuccess {
    body: Uint8Array | string | null
    url: string
    headers: { [header: string]: string }
    statusCode: number
    statusText: string
}

function httpRequestBodyForFetch(body: HttpRequestBody) {
    switch (body.tag) {
        case "Empty": return null
        case "String": return body.value
        case "Uint8Array": return new Blob([body.value])
    }
}
function httpFetch(request: HttpRequest, abortSignal: AbortSignal): Promise<HttpResponse> {
    return fetch(request.url, {
        method: request.method,
        body: httpRequestBodyForFetch(request.body),
        headers: new Headers(request.headers.map(header => {
            // removing the type makes ts think that  tuple: string[]
            const tuple: [string, string] = [header.name, header.value]
            return tuple
        })),
        signal: abortSignal
    })
        .then((response: Response) => {
            const headers = Object.fromEntries(response.headers.entries())
            switch (request.expect) {
                case "String": return response.text()
                    .then((x) => ({
                        tag: "Success" as const,
                        value: {
                            url: response.url,
                            headers: headers,
                            statusCode: response.status,
                            statusText: response.statusText,
                            body: x as (string | null | Uint8Array) // without this as ts complains
                        }
                    }))
                case "Bytes": return response.blob()
                    .then(blob => blob.arrayBuffer())
                    .then((x) => ({
                        tag: "Success" as const,
                        value: {
                            url: response.url,
                            headers: headers,
                            statusCode: response.status,
                            statusText: response.statusText,
                            body: new Uint8Array(x)
                        }
                    }))
                case "Whatever": return {
                    tag: "Success" as const,
                    value: {
                        url: response.url,
                        headers: headers,
                        statusCode: response.status,
                        statusText: response.statusText,
                        body: null
                    }
                }
            }
        })
        .catch(error => { return { tag: "Error", value: error } })
}

// helpers

function warn(warning: string) {
    console.warn(warning + " (lue-bird/elm-state-interface-experimental)")
}
function notifyOfUnknownMessageKind(messageTag: string) {
    notifyOfBug("Unknown message kind " + messageTag + " from elm. The associated js implementation is missing")
}
function notifyOfBug(bugDescription: string) {
    console.error("bug: " + bugDescription + ". Please open an issue on github.com/lue-bird/elm-state-interface-experimental")
}
