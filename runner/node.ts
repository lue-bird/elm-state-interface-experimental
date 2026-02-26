import * as fs from "node:fs"
import * as path from "node:path"
import * as http from "node:http"
import * as timers from "node:timers"
import * as child_process from "node:child_process"
import * as os from "node:os"
// https://github.com/DefinitelyTyped/DefinitelyTyped/discussions/57677
import { default as process } from "node:process"
import { Buffer } from "node:buffer"


export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void,
        unsubscribe: (callback: (fromElm: any) => void) => void
    }
    fromJs: { send: (toElm: any) => void }
}

function showCursor() {
    if (process.stdout.isTTY) {
        process.stdout.write("\u{001B}[?25h")
    }
}
export function programStart(appConfig: { ports: ElmPorts }) {
    process.addListener("beforeExit", (_event) => {
        showCursor() // show cursor
    })
    process.addListener("SIGINT", (_event) => {
        abortControllers.forEach((abortController) => {
            try {
                abortController.abort()
            } catch (errorOnAbort) {
                console.warn("forceful abort of an operation lead to an error ", errorOnAbort)
            }
        })
        appConfig.ports.toJs.unsubscribe(listenToElm)
        showCursor()
    })
    function listenToElm(fromElm: { id: string, diff: { tag: "Add" | "Remove", value: any } }) {
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
    function interfaceDiffImplementation(tag: "Add" | "Remove", sendToElm: (v: any) => void, id: string): ((config: any) => void) {
        switch (tag) {
            case "Add": return (config: { tag: string, value: any }) => {
                const abortController = new AbortController()
                abortControllers.set(id, abortController)
                interfaceAddImplementation(config.tag, sendToElm, abortController.signal)(config.value)
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
                if (process.stdin.setRawMode !== undefined) {
                    process.stdin.setRawMode(false)
                }
                function listen(buffer: Buffer) {
                    sendToElm(buffer.toString())
                }
                process.stdin.addListener("data", listen)
                abortSignal.addEventListener("abort", (_event) => {
                    process.stdin.removeListener("data", listen)
                    if (process.stdin.unref !== undefined) {
                        process.stdin.unref()
                    }
                })
            }
            case "StandardInRawListen": return (_config: null) => {
                if (process.stdin.setRawMode !== undefined) {
                    process.stdin.setRawMode(true)
                }
                function dataListen(buffer: Buffer) {
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
                        showCursor()
                    } else {
                        sendToElm({ tag: "StreamDataReceived", value: stringInput })
                    }
                }
                function endListen() {
                    sendToElm({ tag: "StreamDataEndReached", value: null })
                }
                process.stdin.addListener("data", dataListen)
                process.stdin.addListener("end", endListen)
                abortSignal.addEventListener("abort", (_event) => {
                    process.stdin.removeListener("data", dataListen)
                    process.stdin.removeListener("end", endListen)
                    if (process.stdin.setRawMode !== undefined) {
                        process.stdin.setRawMode(false)
                    }
                    if (process.stdin.unref !== undefined) {
                        process.stdin.unref()
                    }
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
            case "HomeDirectoryPathRequest": return (_config: null) => {
                queueAbortable(abortSignal, () => {
                    sendToElm(os.homedir())
                })
            }
            case "NullDevicePathRequest": return (_config: null) => {
                queueAbortable(abortSignal, () => {
                    sendToElm(os.devNull)
                })
            }
            case "LaunchArgumentsRequest": return (_config: null) => {
                queueAbortable(abortSignal, () => {
                    sendToElm(process.argv)
                })
            }
            case "EnvironmentVariablesRequest": return (_config: null) => {
                queueAbortable(abortSignal, () => {
                    sendToElm(process.env)
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
                            dataAsciiString: bytesToAsciiString(Buffer.concat(dataChunks))
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
                dataAsciiString: string
            }) => {
                const response: HttpServerResponse = {
                    statusCode: config.statusCode,
                    headers: config.headers,
                    data: asciiStringToBytes(config.dataAsciiString)
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
                    timers.setInterval(
                        () => { sendToElm(Date.now()) },
                        config.milliSeconds
                    )
                abortSignal.addEventListener("abort", _event => {
                    timers.clearInterval(timePeriodicallyListenId)
                })
            }
            case "TimeOnce": return (config: { pointInTime: number }) => {
                const timeOnceId =
                    timers.setTimeout(
                        () => { sendToElm(Date.now()) },
                        config.pointInTime - Date.now()
                    )
                abortSignal.addEventListener("abort", _event => {
                    timers.clearTimeout(timeOnceId)
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
            case "FileWrite": return (write: { contentAsciiString: string, path: string }) => {
                fileWrite(write, sendToElm, abortSignal)
            }
            case "FileRequest": return (path: string) => {
                fs.promises.readFile(path, { signal: abortSignal })
                    .then((contentBuffer) => {
                        sendToElm({ tag: "Ok", value: bytesToAsciiString(contentBuffer) })
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
                const retryIntervalId =
                    timers.setInterval(
                        () => {
                            if (fs.existsSync(path)) {
                                timers.clearInterval(retryIntervalId)
                                watchPath(path, abortSignal, sendToElm)
                            }
                        },
                        2000
                    )
                abortSignal.addEventListener("abort", (_event) => {
                    timers.clearInterval(retryIntervalId)
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
            case "SubProcessSpawn": return (config: {
                command: string,
                arguments: Array<string>,
                workingDirectoryPath: string,
                environmentVariables: { [key: string]: string },
            }) => {
                const subProcess = subProcessGetExistingOrSpawn(config)

                function exitListen(code: number) {
                    sendToElm({ tag: "SubProcessExited", value: code })
                }
                subProcess.addListener("exit", exitListen)

                subProcess.addListener("error", (error) => {
                    if (abortSignal.aborted) {
                        // error is expected due to abort
                    } else {
                        warn("sub-process failed: " + error.message)
                    }
                })

                function standardErrorDataListen(buffer: Buffer) {
                    sendToElm({
                        tag: "SubProcessStandardErrEvent",
                        value: { tag: "StreamDataReceived", value: buffer.toString() }
                    })
                }
                function standardErrorEndListen() {
                    sendToElm({
                        tag: "SubProcessStandardErrEvent",
                        value: { tag: "StreamDataEndReached", value: null }
                    })
                }
                subProcess.stderr.addListener("data", standardErrorDataListen)
                subProcess.stderr.addListener("end", standardErrorEndListen)

                function standardOutDataListen(buffer: Buffer) {
                    sendToElm({
                        tag: "SubProcessStandardOutEvent",
                        value: { tag: "StreamDataReceived", value: bytesToAsciiString(buffer) }
                    })
                }
                function standardOutEndListen() {
                    sendToElm({
                        tag: "SubProcessStandardOutEvent",
                        value: { tag: "StreamDataEndReached", value: null }
                    })
                }
                subProcess.stdout.addListener("data", standardOutDataListen)
                subProcess.stdout.addListener("end", standardOutEndListen)

                abortSignal.addEventListener("abort", (_event) => {
                    subProcess.removeListener("exit", exitListen)
                    subProcess.stderr.removeListener("data", standardErrorDataListen)
                    subProcess.stderr.removeListener("end", standardErrorEndListen)
                    subProcess.stderr.removeListener("data", standardOutDataListen)
                    subProcess.stderr.removeListener("end", standardOutEndListen)
                    subProcess.kill()
                    subProcesses.delete(subProcessKey(config))
                })
            }
            case "SubProcessStandardInWrite": return (config: {
                command: string,
                arguments: Array<string>,
                workingDirectoryPath: string,
                environmentVariables: { [key: string]: string },
                data: string,
            }) => {
                const addressedSubProcess = subProcessGetExistingOrSpawn(config)
                if (addressedSubProcess === undefined) {
                    warn("tried to write to standard in of a sub-process that hasn't been spawned, yet")
                } else {
                    addressedSubProcess.stdin.write(asciiStringToBytes(config.data))
                }
            }
            default: return (_config: any) => {
                notifyOfUnknownMessageKind("Add." + tag)
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
/** Use together with `subProcessKey` */
const subProcesses: Map<
    /* JSON.stringify({ command: string,
        arguments: Array<string>,
        workingDirectoryPath: string,
        environmentVariables: { [key: string]: string }
    })
    */
    string,
    child_process.ChildProcessWithoutNullStreams
> =
    new Map()

function subProcessKey(config: {
    command: string,
    arguments: Array<string>,
    workingDirectoryPath: string,
    environmentVariables: { [key: string]: string }
}): string {
    return JSON.stringify(config)
}

function subProcessGetExistingOrSpawn(
    config: {
        command: string,
        arguments: Array<string>,
        workingDirectoryPath: string,
        environmentVariables: { [key: string]: string },
    }
): child_process.ChildProcessWithoutNullStreams {
    const subProcessAssociatedKey = subProcessKey(config)
    const existingSubProcess = subProcesses.get(subProcessAssociatedKey)
    if (existingSubProcess !== undefined) {
        return existingSubProcess
    } else {
        const spawnedSubProcess = child_process.spawn(
            config.command,
            config.arguments,
            {
                cwd: config.workingDirectoryPath,
                env: config.environmentVariables,
                shell: false,
                detached: false,
            }
        )
        subProcesses.set(subProcessAssociatedKey, spawnedSubProcess)
        return spawnedSubProcess
    }
}


type HttpServerResponse = {
    statusCode: number,
    headers: { name: string, value: string }[],
    data: Uint8Array
}


function queueAbortable(abortSignal: AbortSignal, action: () => void) {
    const immediateId = timers.setImmediate(action)
    abortSignal.addEventListener("abort", _event => {
        timers.clearImmediate(immediateId)
    })
}


function fileWrite(write: { path: string, contentAsciiString: string }, sendToElm: (v: any) => void, abortSignal: AbortSignal) {
    recentlyWrittenToFilePaths.add(write.path)
    fs.promises.writeFile(
        write.path,
        asciiStringToBytes(write.contentAsciiString),
        { signal: abortSignal }
    )
        .then(() => {
            sendToElm({ tag: "Ok", value: null })
            timers.setTimeout(
                () => {
                    recentlyWrittenToFilePaths.delete(write.path)
                },
                100
            )
        })
        .catch((error) => {
            sendToElm({ tag: "Err", value: error })
            timers.setTimeout(
                () => {
                    recentlyWrittenToFilePaths.delete(write.path)
                },
                100
            )
        })
    abortSignal.addEventListener("abort", (_event) => {
        timers.setTimeout(
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
                        timers.setTimeout(
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
    bodyAsciiString: string | null
}
type HttpResponse =
    | {
        tag: "Ok",
        value: {
            statusCode: number,
            statusText: string,
            headers: { name: string, value: string }[],
            bodyAsciiString: string
        }
    }
    | { tag: "Err", value: any }

function httpFetch(request: HttpRequest, abortSignal: AbortSignal): Promise<HttpResponse> {
    return fetch(request.url, {
        method: request.method,
        body:
            request.bodyAsciiString === null ?
                null
                : new Uint8Array(asciiStringToBytes(request.bodyAsciiString)),
        headers: new Headers(request.headers.map(header => {
            // removing the type makes ts think that  tuple: string[]
            const tuple: [string, string] = [header.name, header.value]
            return tuple
        })),
        signal: abortSignal
    })
        .then((response) =>
            response
                .arrayBuffer()
                // use intermediate ArrayBuffer bc chromium does not support .bytes() yet
                // https://developer.mozilla.org/en-US/docs/Web/API/Blob/bytes
                .then((bodyArrayBuffer) => ({
                    tag: "Ok" as const,
                    value: {
                        statusCode: response.status,
                        statusText: response.statusText,
                        headers:
                            Array.from(response.headers.entries())
                                .map(([name, value]) => ({ name: name, value: value })),
                        bodyAsciiString:
                            bytesToAsciiString(new Uint8Array(bodyArrayBuffer))
                    }
                }))
        )
        .catch((error) => ({ tag: "Err" as const, value: error }))
}


function asciiStringToBytes(string: string): Uint8Array {
    const result = new Uint8Array(string.length);
    for (let i = 0; i < string.length; i++) {
        result[i] = string.charCodeAt(i)
    }
    return result;
}
function bytesToAsciiString(bytes: Uint8Array): string {
    let result = ""
    for (let i = 0; i < bytes.length; i++) {
        result += String.fromCharCode(bytes[i] as number)
    }
    return result
}


function warn(warning: string) {
    console.warn(warning + " (lue-bird/elm-state-interface-experimental)")
}
function notifyOfUnknownMessageKind(messageTag: string) {
    notifyOfBug("unknown message kind " + messageTag + " from elm. The associated js implementation is missing")
}
function notifyOfBug(bugDescription: string) {
    console.error("bug: " + bugDescription + ". Please open an issue on github.com/lue-bird/elm-state-interface-experimental")
}
