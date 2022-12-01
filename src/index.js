'use strict';

import "./tailwind.css";
require("./theme.css");
require("./styles.scss");
import '@github/clipboard-copy-element';
import { createConsumer } from 'actioncable-jwt'

const {Elm} = require('./Main');

const bytesKey = "bytes"
const bearerKey = "bearer"


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
console.log(apiUrl)


const flags = {
    bytes: rememberedBytes(),
    bearer: JSON.parse(localStorage.getItem(bearerKey)),
    apiUrl: apiUrl
}

var app = Elm.Main.init({flags: flags});


app.ports.toggleDarkMode.subscribe(() => { toggleDarkMode() });


// PWA STUFF

import { PushApp } from "./push.js";

PushApp.init()
    .then((status) => { console.log(status) ; app.ports.onPushChange.send(status) } );

const pushSubscribe = () => { PushApp.subscribe().then((status) => { app.ports.onPushChange.send(status) } ) }
const pushUnsubscribe = () => { PushApp.unsubscribe().then((status) => { app.ports.onPushChange.send(status) } ) }

app.ports.subscribePush.subscribe(pushSubscribe);
app.ports.unsubscribePush.subscribe(pushUnsubscribe);

// PWA STAFF





const Sockets = {
    games: {}
};


const initConsumer = () => {
    let bearer = JSON.parse(localStorage.getItem(bearerKey));
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

app.ports.storeSession.subscribe(function (val) {
    if (val === null) { // logout
        localStorage.removeItem(bearerKey);
    } else {
        localStorage.setItem(bearerKey, JSON.stringify(val));
    }
    initConsumer();
    // https://stackoverflow.com/questions/779379/why-is-settimeoutfn-0-sometimes-useful
    setTimeout(function () { app.ports.onSessionChange.send(val); }, 0);
});


console.log(Sockets)
console.log(Sockets.consumer)




// Listen for localStorage changes
window.addEventListener("storage", function (event) {
    if (event.storageArea === localStorage && event.key === bearerKey) {
        app.ports.onSessionChange.send(JSON.parse(event.newValue));
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
