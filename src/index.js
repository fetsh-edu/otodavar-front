'use strict';

import "./tailwind.css";
require("./theme.css");
require("./styles.scss");
import '@github/clipboard-copy-element';
import { createConsumer } from 'actioncable-jwt'
import TelegramButton from './telegram-button.js';


const {Elm} = require('./Main');

const bytesKey = "bytes"
const bearerKey = "bearer"
const infoKey = "info"


function getTheme() {
    var theme="light";    //default to light
    if(localStorage.getItem("theme") && localStorage.getItem("theme") == "dark"){
        var theme = "dark";
    } else if(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
        var theme = "dark";
    }
    return theme;
}

function setDarkMode() {
    if (getTheme() == "dark") {
        document.documentElement.classList.remove("light")
        document.documentElement.classList.add("dark")
    } else {
        document.documentElement.classList.remove("dark")
        document.documentElement.classList.add("light")
    }
}
function toggleDarkMode() {
    if (getTheme() == "dark") {
        localStorage.setItem("theme", "light")
    } else {
        localStorage.setItem("theme", "dark")
    }
    setDarkMode();
}

setDarkMode();


// TODO. Check how it should be done in JS
let apiUrl = "https://otodavar-api.fetsh.me";
if (typeof process !== 'undefined') {
    if (typeof process.env !== 'undefined') {
        if (typeof process.env.API_URL !== 'undefined') {
            apiUrl = process.env.API_URL;
        }
    }
}

const buildBearerInfo = (a, b) => {
    const bearer_ = JSON.parse(a)
    const info_ = JSON.parse(b)
    if (bearer_ === null) {
        return null;
    } else if (typeof bearer_ === "string") {
        return { bearer: bearer_, info: info_ };
    } else if (typeof bearer_ === "object") {
        if (info_) {
            bearer_.info = info_
        }
        return bearer_;
    }
}

const flags = {
    bytes: rememberedBytes(),
    bearer: buildBearerInfo(localStorage.getItem(bearerKey), localStorage.getItem(infoKey)),
    apiUrl: apiUrl
}

var app = Elm.Main.init({flags: flags});


app.ports.toggleDarkMode.subscribe(() => { toggleDarkMode() });


// PWA STUFF

import { PushApp } from "./push.js";

// Send initial permission status
let permission = PushApp.getPermission()
console.log("Initial permission", permission)
app.ports.receivedPermission.send( permission )

if (permission == "granted") {
    PushApp.getSubscription().then((subscription) => { app.ports.onPushChange.send(subscription) } )
}

// Ask for permission and send it back;
app.ports.requestPermission.subscribe(() => {
    PushApp.requestPermission().then((permission) => app.ports.receivedPermission.send(permission))
})
app.ports.subscribePush.subscribe(() => { PushApp.subscribe().then((subscription) => { app.ports.onPushChange.send(subscription) })});
app.ports.unsubscribePush.subscribe(() => { PushApp.unsubscribe().then((subscription) => { app.ports.onPushChange.send(subscription) })});


//app.ports.subscribePush.subscribe(() => {
//    PushApp.subscribe().then((status) => { app.ports.onPushChange.send(status) } )
//});

//PushApp.init().then((status) => { app.ports.onPushChange.send(status) } );
//
//const pushUnsubscribe = () => { PushApp.unsubscribe().then((status) => { app.ports.onPushChange.send(status) } ) }
//
//app.ports.unsubscribePush.subscribe(pushUnsubscribe);

// PWA STAFF





const Sockets = {
    games: {}
};


const initConsumer = () => {
    let bearer = buildBearerInfo(localStorage.getItem(bearerKey), localStorage.getItem(infoKey));
    if (bearer === null) {
        console.log("No consumer for logged out.");
        if (Sockets.consumer) {
            if (Sockets.notificationsSub) {
                Sockets.consumer.subscriptions.remove(Sockets.notificationsSub);
                console.log("Removing notifications subscription;")
            }
            if (Sockets.gameSub) {
                Sockets.consumer.subscriptions.remove(Sockets.gameSub);
                console.log("Removing Game subscription;")
            }
        }
    } else {
        console.log("Initializing new consumer");
        var host = apiUrl.replace(/^https:\/\//, '');
        Sockets.consumer = createConsumer("wss://" + host + "/cable", bearer.bearer.split(" ")[1]);
//        var host = apiUrl.replace(/^https:\/\//, '').replace(/:[0-9]{4}$/, '');
//        Sockets.consumer = createConsumer("wss://otodavar-api.fetsh.me/cable", bearer.bearer.split(" ")[1]);
//        Sockets.consumer = createConsumer("wss://localhost:3001/cable", bearer.bearer.split(" ")[1]);

        Sockets.notificationsSub = Sockets.consumer.subscriptions.create({channel: "NotificationsChannel"}, {
            initialized: function() { console.log("cable initialized") },
            connected: function() { console.log("cable: connected") },             // onConnect
            disconnected: function() { console.log("cable: disconnected") },       // onDisconnect
            received: (data) => {
                console.log("cable received: ", data);
                app.ports.onNotification.send(data);
            }
        })
    }
}

initConsumer();

app.ports.subscribeToGame.subscribe(function (game_uid) {
    if (Sockets.games[game_uid]) {

    } else {
        Object.values(Sockets.games).forEach(sub =>
            Sockets.consumer.subscriptions.remove(sub)
        )
        Sockets.games = {}
        Sockets.games[game_uid] = Sockets.consumer.subscriptions.create({channel: "GameChannel", game: game_uid}, {
            initialized: function() { console.log("Game cable initialized") },
            connected: function() { console.log("Game cable: connected") },             // onConnect
            disconnected: function() { console.log("Gmae cable: disconnected") },       // onDisconnect
            received: (data) => {
                console.log("Game cable received: ", data);
                app.ports.onGameMessage.send(data);
            }
        })
    }
})


app.ports.storeUserInfo.subscribe(function (val) {
    console.log(val)
    if (val === null) {
        localStorage.removeItem(infoKey);
    } else {
        localStorage.setItem(infoKey, JSON.stringify(val));
    }
})

app.ports.storeSession.subscribe(function (val) {
    if (val === null) { // logout
        localStorage.removeItem(bearerKey);
    } else {
        localStorage.setItem(bearerKey, JSON.stringify(val.bearer));
        localStorage.setItem(infoKey, JSON.stringify(val.info));
    }
    initConsumer();
    // https://stackoverflow.com/questions/779379/why-is-settimeoutfn-0-sometimes-useful
    setTimeout(function () { app.ports.onSessionChange.send(val); }, 0);
});


// Listen for localStorage changes
window.addEventListener("storage", function (event) {
    if (event.storageArea === localStorage && event.key === bearerKey) {
        app.ports.onSessionChange.send(
            buildBearerInfo(event.newValue, localStorage.getItem(infoKey))
        );
    }
}, false);

function rememberedBytes() {
    const bytes = localStorage.getItem(bytesKey);
    return bytes ? bytes.split(",").map(x => parseInt(x,10)) : null;
}

/* Generate high entropy random bytes using the Web Crypto API and
remember them so that they are preserved between redirections. This
allows to protect for XSS & authorization code attacks */
app.ports.genRandomBytes.subscribe(n => {
    const buffer = new Uint8Array(n);
    crypto.getRandomValues(buffer);
    const bytes = Array.from(buffer);
    localStorage.setItem(bytesKey, bytes);
    app.ports.randomBytes.send(bytes);
});
