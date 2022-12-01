const applicationServerPublicKey = 'BJG6BHoYQJAAFGfjzR1O5TNIOZaJqrS5obFgZ6re__GH4oeli1Xg7q4JQAnJXLEqMCvhOx79KoMsKWDVNAx032g';
const applicationServerKeyEncoded = urlB64ToUint8Array(applicationServerPublicKey);

const DENIED        = "denied";
const SUBSCRIBED    = "subscribed";
const UNSUBSCRIBED  = "unsubscribed";
const ERROR         = "error";
const NOT_SUPPORTED = "not_supported";
const NOT_ASKED     = "not_asked";


const checkDenied = () => { return new Promise(function(myResolve, myReject)  {
    if (Notification.permission === 'denied') {
        myReject(DENIED);
    } else {
        myResolve();
    }
})}

const checkSupport = () => { return new Promise(function(myResolve, myReject)  {
    if ('serviceWorker' in navigator && 'PushManager' in window) {
        myResolve()
    } else {
        myReject(NOT_SUPPORTED);
    }
})}

const registerWorker = () => { return new Promise(function(myResolve, myReject)  {
    navigator.serviceWorker.register('/sw.js').then(
        (registration) => myResolve(registration),
        (error) => myReject(ERROR)
    )
})}

const saveRegistration = (registration) => { PushApp.registration = registration; return registration; }

const getSubscription = (registration) => {
    return new Promise(function(myResolve, myReject)  {
        registration.pushManager.getSubscription().then(
            (subscription) => myResolve(subscription),
            (error) => myReject(ERROR)
        )
    })
}

const subscriptionToStatus = (sub) => {
     if (!(sub === null)) {
         return SUBSCRIBED
     } else {
         return UNSUBSCRIBED
     }
}

const getRegistration = () => { return new Promise(function(myResolve, myReject){
    if (PushApp.registration) {
        myResolve(PushApp.registration)
    } else {
        checkSupport()
            .then(registerWorker)
            .then( (registration) => saveRegistration(registration) )
            .then( (registration) => { myResolve(registration) } )
            .catch( (e) => myReject(ERROR) )
    }
}) }

const subscribe = (registration) => {
    return new Promise(function(myResolve, myReject) {
        registration.pushManager.subscribe({
            userVisibleOnly: true,
            applicationServerKey: applicationServerKeyEncoded
        })
        .then(
            (subscription) => myResolve(subscription),
            (error) => myReject(ERROR)
        )
    });
};

const unsubscribe = (subscription) => new Promise(function(myResolve, myReject) {
    if (subscription) {
        subscription.unsubscribe().then(
            () => myResolve(),
            (e) => myReject(ERROR)
        )
    } else {
        myResolve()
    }
});


export const PushApp = {
    registration: null,
    status: NOT_ASKED,
    getStatus: () => {
        return checkDenied().then(
            () => { return PushApp.status },
            (denied) => { return denied }
        )
    },
    init: () => {
        return getRegistration()
            .then( (registration) => getSubscription(registration) )
            .then( (subscription) => { PushApp.status = subscriptionToStatus(subscription) } )
            .catch( (val) => PushApp.status = val )
            .then(() => PushApp.getStatus())
    },
    subscribe: () => {
        return getRegistration()
            .then( (registration) => subscribe(registration) )
            .then( (subscription) => { PushApp.status = subscriptionToStatus(subscription) } )
            .catch( (val) => PushApp.status = val )
            .then(() => PushApp.getStatus())
    },
    unsubscribe: () => {
        return getRegistration()
            .then( (registration) => getSubscription(registration) )
            .then( (subscription) => unsubscribe(subscription) )
            .then( () => { PushApp.status = subscriptionToStatus(null) } )
            .catch( (val) => PushApp.status = val )
            .then(() => PushApp.getStatus())
    }
};

function urlB64ToUint8Array(base64String) {
  const padding = '='.repeat((4 - base64String.length % 4) % 4);
  const base64 = (base64String + padding)
    .replace(/\-/g, '+')
    .replace(/_/g, '/');

  const rawData = window.atob(base64);
  const outputArray = new Uint8Array(rawData.length);

  for (let i = 0; i < rawData.length; ++i) {
    outputArray[i] = rawData.charCodeAt(i);
  }
  return outputArray;
}