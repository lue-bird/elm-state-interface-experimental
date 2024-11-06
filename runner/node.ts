import * as fs from "node:fs"
import * as path from "node:path"
import * as http from "node:http"

export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void,
        unsubscribe: (callback: (fromElm: any) => void) => void
    }
    fromJs: { send: (toElm: any) => void }
}

export function programStart(appConfig: { ports: ElmPorts }) {
    process.addListener("exit", (_event) => {
        process.stdout.write("\u{001B}[?25h") // show cursor
    })
    process.addListener("SIGTERM", (_event) => {
        process.stdout.write("\u{001B}[?25h") // show cursor
    })
    function listenToElm(fromElm: { id: string, diff: { tag: "Add" | "Edit" | "Remove", value: any } }) {
        // uncomment for debugging
        // (process.stdout as any)?._handle?.setBlocking(true) // make log sync https://github.com/nodejs/node/issues/11568#issuecomment-282765300
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
                    try {
                        abortController.abort()
                    } catch (errorOnAbort) {
                        console.warn(
                            "Removing an interface aborted an operation which lead to an error:",
                            errorOnAbort,
                            "Try to keep such interfaces alive until you receive confirmation they completed"
                        )
                    }
                    abortControllers.delete(id)
                } else {
                    notifyOfBug("trying to remove an interface that was already aborted")
                }
            }
        }
    }
    function interfaceAddImplementation(tag: string, sendToElm: (v: any) => void, abortSignal: AbortSignal): ((config: any) => void) {
        switch (tag) {
            case "StandardInListen": return (_config: null) => {
                process.stdin.setRawMode(false)
                function listen(buffer: Buffer) {
                    sendToElm(buffer.toString())
                }
                process.stdin.addListener("data", listen)
                abortSignal.addEventListener("abort", (_event) => {
                    process.stdin.removeListener("data", listen)
                    process.stdin.unref()
                })
            }
            case "StandardInRawListen": return (_config: null) => {
                process.stdin.setRawMode(true)
                function listen(buffer: Buffer) {
                    const stringInput = buffer.toString()
                    if (stringInput == "\u0003") { // ctrl+c
                        abortControllers.forEach((abortController) => {
                            try {
                                abortController.abort()
                            } catch (errorOnAbort) {
                                console.warn("forceful abort of an operation lead to an error ", errorOnAbort)
                            }
                        })
                        appConfig.ports.toJs.unsubscribe(listenToElm)
                        process.stdout.write("\u{001B}[?25h\n") // show cursor
                    } else {
                        sendToElm(stringInput)
                    }
                }
                process.stdin.addListener("data", listen)
                abortSignal.addEventListener("abort", (_event) => {
                    process.stdin.removeListener("data", listen)
                    process.stdin.setRawMode(false)
                    process.stdin.unref()
                })
            }
            case "StandardOutWrite": return (text: string) => {
                process.stdout.write(text)
            }
            case "StandardErrWrite": return (text: string) => {
                process.stderr.write(text)
            }
            case "WorkingDirectoryPathRequest": return (_config: null) => {
                queueAbortable(abortSignal, () => {
                    sendToElm(process.cwd())
                })
            }
            case "LaunchArgumentsRequest": return (_config: null) => {
                queueAbortable(abortSignal, () => {
                    sendToElm(process.argv)
                })
            }
            case "Exit": return (code: number) => {
                process.exitCode = code
            }
            case "ProcessTitleSet": return (newTitle: string) => {
                process.title = newTitle
            }
            case "TerminalSizeRequest": return (_config: null) => {
                queueAbortable(abortSignal, () => {
                    sendToElm({ lines: process.stdout.rows, columns: process.stdout.columns })
                })
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
            case "HttpRequestSend": return (config: HttpRequest) => {
                httpFetch(config, abortSignal).then(sendToElm)
            }
            case "HttpRequestListen": return (config: { port: number }) => {
                const server = http.createServer()
                server.addListener("request", (request, responseBuilder) => {
                    let dataChunks: Uint8Array[] = []
                    request.addListener("data", (dataChunk) => {
                        dataChunks.push(dataChunk)
                    })
                    sendToElm({
                        tag: "HttpRequestReceived", value: {
                            method: request.method,
                            headers:
                                Object.entries(request.headers)
                                    .map(([name, value]) => ({ name: name, value: value })),
                            data: Array.from(Buffer.concat(dataChunks))
                        }
                    })
                    function sendResponse(response: HttpServerResponse) {
                        responseBuilder.writeHead(
                            response.statusCode,
                            Object.fromEntries(response.headers.map((header) => {
                                // removing the type makes ts think that  tuple: string[]
                                const tuple: [string, string] = [header.name, header.value]
                                return tuple
                            }))
                        )
                        responseBuilder.write(response.data)
                        responseBuilder.end()
                        sendToElm({ tag: "HttpResponseSent", value: null })
                    }
                    const responseAlreadyWaiting =
                        httpResponsesAwaitingRequest.get(config.port)
                    if (responseAlreadyWaiting === undefined) {
                        httpRequestsAwaitingResponse.set(config.port, sendResponse)
                    } else {
                        sendResponse(responseAlreadyWaiting)
                    }
                })
                server.addListener("error", (error: { code: number, message: string }) => {
                    sendToElm({ tag: "HttpServerFailed", value: { code: error.code, message: error.message } })
                })
                server.listen(config.port)
                sendToElm({ tag: "HttpServerOpened", value: null })

                abortSignal.addEventListener("abort", (_event) => {
                    server.close()
                })
            }
            case "HttpResponseSend": return (config: {
                port: number,
                statusCode: number,
                headers: { name: string, value: string }[],
                dataUnsignedInt8s: number[]
            }) => {
                const response: HttpServerResponse = {
                    statusCode: config.statusCode,
                    headers: config.headers,
                    data: Buffer.from(config.dataUnsignedInt8s)
                }
                const httpRequestAwaitingResponse = httpRequestsAwaitingResponse.get(config.port)
                if (httpRequestAwaitingResponse === undefined) {
                    httpResponsesAwaitingRequest.set(config.port, response)
                    abortSignal.addEventListener("abort", (_event) => {
                        if (httpResponsesAwaitingRequest.get(config.port) === response) {
                            httpResponsesAwaitingRequest.delete(config.port)
                        }
                    })
                } else {
                    httpRequestAwaitingResponse(response)
                    httpRequestsAwaitingResponse.delete(config.port)
                }
            }
            case "TimePosixRequest": return (_config: null) => {
                queueAbortable(abortSignal, () => {
                    sendToElm(Date.now())
                })
            }
            case "TimezoneOffsetRequest": return (_config: null) => {
                // Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L38-L52
                queueAbortable(abortSignal, () => {
                    sendToElm(new Date().getTimezoneOffset())
                })
            }
            case "TimezoneNameRequest": return (_config: null) => {
                queueAbortable(abortSignal, () => {
                    sendToElm(Intl.DateTimeFormat().resolvedOptions().timeZone)
                })
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
                    clearTimeout(timeOnceId)
                })
            }
            case "RandomUnsignedInt32sRequest": return (config: number) => {
                queueAbortable(abortSignal, () => {
                    sendToElm(Array.from(crypto.getRandomValues(new Uint32Array(config))))
                })
            }
            case "DirectoryMake": return (path: string) => {
                fs.promises.mkdir(path, { recursive: true })
                    .then((_createdPath) => {
                        if (!abortSignal.aborted) {
                            sendToElm({ tag: "Ok", value: null })
                        }
                    })
                    .catch((error) => {
                        if (!abortSignal.aborted) {
                            sendToElm({ tag: "Err", value: error })
                        }
                    })
            }
            case "FileRemove": return (path: string) => {
                fs.promises.unlink(path)
                    .then(() => { })
                    .catch((error) => {
                        console.warn("failed to unlink file", error)
                    })
            }
            case "FileWrite": return (write: { contentUnsignedInt8s: number[], path: string }) => {
                fileWrite(write, sendToElm, abortSignal)
            }
            case "FileRequest": return (path: string) => {
                fs.promises.readFile(path, { signal: abortSignal })
                    .then((contentBuffer) => {
                        sendToElm({ tag: "Ok", value: Array.from(contentBuffer) })
                    })
                    .catch((error) => {
                        if (!abortSignal.aborted) {
                            sendToElm({ tag: "Err", value: error })
                        }
                    })
            }
            case "FileInfoRequest": return (path: string) => {
                fs.promises.stat(path)
                    .then((stats) => {
                        if (!abortSignal.aborted) {
                            sendToElm({
                                byteCount: stats.size,
                                kind: stats.isDirectory() ? "Directory" : "File",
                                lastContentChangePosixMilliseconds: stats.mtimeMs
                            })
                        }
                    })
                    .catch((_enoend) => {
                        if (!abortSignal.aborted) {
                            sendToElm(null)
                        }
                    })
            }
            case "FileChangeListen": return (path: string) => {
                // if you have a nice solution to wait for
                // files popping into existence, please show me
                const retryIntervalId = setInterval(
                    () => {
                        if (fs.existsSync(path)) {
                            clearInterval(retryIntervalId)
                            watchPath(path, abortSignal, sendToElm)
                        }
                    },
                    2000
                )
                abortSignal.addEventListener("abort", (_event) => {
                    clearInterval(retryIntervalId)
                })
            }
            case "DirectorySubPathsRequest": return (path: string) => {
                fs.promises.readdir(path, { recursive: true })
                    .then((subNames) => {
                        if (!abortSignal.aborted) {
                            sendToElm({ tag: "Ok", value: subNames })
                        }
                    })
                    .catch((error) => {
                        if (!abortSignal.aborted) {
                            sendToElm({ tag: "Err", value: error })
                        }
                    })
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

const abortControllers: Map<string, AbortController> =
    new Map()
const httpRequestsAwaitingResponse: Map<number /* port */, ((response: HttpServerResponse) => void)> =
    new Map()
const httpResponsesAwaitingRequest: Map<number /* port */, HttpServerResponse> =
    new Map()
// prevents watching and falling into duplicate work or even an infinite loop
const recentlyWrittenToFilePaths: Set<string> =
    new Set()


type HttpServerResponse = {
    statusCode: number,
    headers: { name: string, value: string }[],
    data: Buffer
}


function queueAbortable(abortSignal: AbortSignal, action: () => void) {
    const immediateId = setImmediate(action)
    abortSignal.addEventListener("abort", _event => {
        clearImmediate(immediateId)
    })
}


function fileWrite(write: { path: string, contentUnsignedInt8s: number[] }, sendToElm: (v: any) => void, abortSignal: AbortSignal) {
    recentlyWrittenToFilePaths.add(write.path)
    fs.promises.writeFile(
        write.path,
        Uint8Array.from(write.contentUnsignedInt8s),
        { signal: abortSignal }
    )
        .then(() => {
            sendToElm({ tag: "Ok", value: null })
            setTimeout(
                () => {
                    recentlyWrittenToFilePaths.delete(write.path)
                },
                100
            )
        })
        .catch((error) => {
            sendToElm({ tag: "Err", value: error })
            setTimeout(
                () => {
                    recentlyWrittenToFilePaths.delete(write.path)
                },
                100
            )
        })
    abortSignal.addEventListener("abort", (_event) => {
        setTimeout(
            () => {
                recentlyWrittenToFilePaths.delete(write.path)
            },
            100
        )
    })
}

function watchPath(
    pathToWatch: string,
    abortSignal: AbortSignal,
    sendToElm: (event: { tag: "Removed" | "AddedOrChanged", value: string }) => void
) {
    // most editors chunk up their file edits in 2 or 4, see
    // https://stackoverflow.com/questions/12978924/fs-watch-fired-twice-when-i-change-the-watched-file
    // https://thisdavej.com/how-to-watch-for-file-changes-in-node-js/
    // so we buffer until the latest chunk after 25ms still has no other chunk arrive in the meantime
    let timeoutIdWaitingForLastChunk: NodeJS.Timeout | null = null

    fs.watch(
        pathToWatch,
        { recursive: true, signal: abortSignal },
        (_event, fileName) => {
            if (fileName !== null) {
                const fullPath =
                    path.basename(pathToWatch) == fileName ?
                        pathToWatch
                        :
                        path.join(pathToWatch, fileName)
                if (!recentlyWrittenToFilePaths.has(fullPath)) {
                    const currentAttemptTimeoutIdWaitingForLastChunk =
                        setTimeout(
                            () => {
                                if (currentAttemptTimeoutIdWaitingForLastChunk === timeoutIdWaitingForLastChunk) {
                                    if (fs.existsSync(fullPath)) {
                                        sendToElm({ tag: "AddedOrChanged", value: fullPath })
                                    } else {
                                        sendToElm({ tag: "Removed", value: fullPath })
                                    }
                                }
                            },
                            25
                        )
                    timeoutIdWaitingForLastChunk = currentAttemptTimeoutIdWaitingForLastChunk
                }
            }
        }
    )
}




interface HttpRequest {
    url: string
    method: string
    headers: { name: string, value: string }[]
    bodyUnsignedInt8s: Uint8Array
}
type HttpResponse =
    | {
        tag: "Success",
        value: {
            statusCode: number,
            statusText: string,
            headers: { name: string, value: string }[],
            bodyUnsignedInt8s: number[]
        }
    }
    | { tag: "Error", value: any }

function httpFetch(request: HttpRequest, abortSignal: AbortSignal): Promise<HttpResponse> {
    return fetch(request.url, {
        method: request.method,
        body:
            request.bodyUnsignedInt8s === null ?
                null
                : new Blob([new Uint8Array(request.bodyUnsignedInt8s)]),
        headers: new Headers(request.headers.map(header => {
            // removing the type makes ts think that  tuple: string[]
            const tuple: [string, string] = [header.name, header.value]
            return tuple
        })),
        signal: abortSignal
    })
        .then((response) =>
            response
                .blob()
                .then((bodyBlob) => bodyBlob.arrayBuffer())
                // use intermediate ArrayBuffer bc chromium does not support .bytes() yet
                // https://developer.mozilla.org/en-US/docs/Web/API/Blob/bytes
                .then((bodyArrayBuffer) => ({
                    tag: "Success" as const,
                    value: {
                        statusCode: response.status,
                        statusText: response.statusText,
                        headers:
                            Array.from(response.headers.entries())
                                .map(([name, value]) => ({ name: name, value: value })),
                        bodyUnsignedInt8s:
                            Array.from(new Uint8Array(bodyArrayBuffer))
                    }
                }))
        )
        .catch((error) => ({ tag: "Error" as const, value: error }))
}



function notifyOfUnknownMessageKind(messageTag: string) {
    notifyOfBug("unknown message kind " + messageTag + " from elm. The associated js implementation is missing")
}
function notifyOfBug(bugDescription: string) {
    console.error("bug: " + bugDescription + ". Please open an issue on github.com/lue-bird/elm-state-interface-experimental")
}
