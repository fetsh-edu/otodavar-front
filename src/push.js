const applicationServerPublicKey = 'BJG6BHoYQJAAFGfjzR1O5TNIOZaJqrS5obFgZ6re__GH4oeli1Xg7q4JQAnJXLEqMCvhOx79KoMsKWDVNAx032g';
const applicationServerKeyEncoded = urlB64ToUint8Array(applicationServerPublicKey);

const DENIED        = { status: "denied" };
const SUBSCRIBED    = (subscription) => { return { status: "subscribed", payload: JSON.stringify(subscription) } };
const UNSUBSCRIBED  = (subscription) => { return { status: "unsubscribed", payload: JSON.stringify(subscription) } };
const ERROR         = { status: "error" };
const ERROR_WORKER  = { status: "error", error: "worker" };
const ERROR_GET_SUB = { status: "error", error: "get_subscription" };
const ERROR_GET_REG = { status: "error", error: "get_registration" };
const ERROR_SUB     = { status: "error", error: "subscribe" };
const ERROR_UNSUB   = { status: "error", error: "unsubscribe" };
const NOT_SUPPORTED = { status: "not_supported" };
const NOT_ASKED     = { status: "not_asked" };


const checkDenied = () => { return new Promise(function(myResolve, myReject)  {
    if (Notification.permission === 'denied') {
        myReject(DENIED);
    } else {
        myResolve();
    }
})}

const getPermission = () => { return Notification.permission }

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
        (error) => myReject(ERROR_WORKER)
    )
})}

const saveRegistration = (registration) => { PushApp.registration = registration; return registration; }

const getSubscription = (registration) => {
    return new Promise(function(myResolve, myReject)  {
        registration.pushManager.getSubscription().then(
            (subscription) => myResolve(subscription),
            (error) => myReject(ERROR_GET_SUB)
        )
    })
}

const subscriptionToStatus = (sub) => {
     if (!(sub === null)) {
        console.log("Subscription", sub)
        return SUBSCRIBED(sub)
     } else {
         return UNSUBSCRIBED(sub)
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
            .catch( (e) => myReject(ERROR_GET_REG) )
    }
}) }

const subscribe = (registration) => {
    return new Promise(function(myResolve, myReject) {
        checkDenied().then(
            () => registration.pushManager.subscribe({
                      userVisibleOnly: true,
                      applicationServerKey: applicationServerKeyEncoded
                  })
                  .then(
                      (subscription) => myResolve(subscription),
                      (error) => myReject(ERROR_SUB)
                  ),
            (e) => myReject(e)
        )
    });
};

const unsubscribe = (subscription) => new Promise(function(myResolve, myReject) {
    if (subscription) {
        subscription.unsubscribe().then(
            () => myResolve(subscription),
            (e) => myReject(ERROR_UNSUB)
        )
    } else {
        myResolve(null)
    }
});


export const PushApp = {
    registration: null,
    status: NOT_ASKED,
    getPermission: getPermission,
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
            .then( (subscription) => { PushApp.status = UNSUBSCRIBED(subscription) } )
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