export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void
    }
    fromJs: { send: (toElm: any) => void }
}

export function programStart(appConfig: { ports: ElmPorts, domElement: Element }) {
    appConfig.ports.toJs.subscribe(function (fromElm: { id: string, diff: { tag: "Add" | "Edit" | "Remove", value: any } }) {
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
    })
    function interfaceDiffImplementation(tag: "Add" | "Edit" | "Remove", sendToElm: (v: any) => void, id: string): ((config: any) => void) {
        switch (tag) {
            case "Add": return (config: { tag: string, value: any }) => {
                const abortController = new AbortController()
                abortControllers.set(id, abortController)
                interfaceAddImplementation(id, config.tag, sendToElm, abortController.signal)(config.value)
            }
            case "Edit": return (config: { tag: string, value: any }) => {
                interfaceEditImplementation(id, config.tag, sendToElm)(config.value)
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
    function interfaceAddImplementation(id: string, tag: string, sendToElm: (v: any) => void, abortSignal: AbortSignal): ((config: any) => void) {
        switch (tag) {
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
                    window.setInterval(
                        () => { sendToElm(Date.now()) },
                        config.milliSeconds
                    )
                abortSignal.addEventListener("abort", _event => {
                    window.clearInterval(timePeriodicallyListenId)
                })
            }
            case "TimeOnce": return (config: { pointInTime: number }) => {
                const timeOnceId =
                    window.setTimeout(
                        () => { sendToElm(Date.now()) },
                        config.pointInTime - Date.now()
                    )
                abortSignal.addEventListener("abort", _event => {
                    window.clearInterval(timeOnceId)
                })
            }
            case "RandomUnsignedInt32sRequest": return (config: number) => {
                sendToElm(Array.from(window.crypto.getRandomValues(new Uint32Array(config))))
            }
            default: return (_config: any) => {
                notifyOfUnknownMessageKind("Add." + tag)
            }
        }
    }
    function interfaceEditImplementation(id: string, tag: string, sendToElm: (v: any) => void): ((config: any) => void) {
        switch (tag) {
            default: return (_config: any) => {
                notifyOfUnknownMessageKind("Edit." + tag)
            }
        }
    }
}

const abortControllers: Map<string, AbortController> = new Map()




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
