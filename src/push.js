const applicationServerPublicKey = 'BJG6BHoYQJAAFGfjzR1O5TNIOZaJqrS5obFgZ6re__GH4oeli1Xg7q4JQAnJXLEqMCvhOx79KoMsKWDVNAx032g';
const applicationServerKeyEncoded = urlB64ToUint8Array(applicationServerPublicKey);

const SUBSCRIBED    = (subscription) => { return { status: "subscribed", payload: JSON.stringify(subscription) } };
const UNSUBSCRIBED  = (subscription) => { return { status: "unsubscribed", payload: JSON.stringify(subscription) } };
const ERROR_WORKER  = { status: "error", error: "worker" };
const ERROR_GET_SUB = { status: "error", error: "get_subscription" };
const ERROR_GET_REG = { status: "error", error: "get_registration" };
const ERROR_SUB     = { status: "error", error: "subscribe" };
const ERROR_UNSUB   = { status: "error", error: "unsubscribe" };

const supported = (("Notification" in window) && ('serviceWorker' in navigator) && ('PushManager' in window))

const getPermission = () => {
    if (!supported) {
        return "not_supported";
    } else {
        return Notification.permission
    }
}

const requestPermission = () => {
    return new Promise(function(myResolve, myReject) {
        if (!supported) {
            myResolve("not_supported")
        } else if (Notification.permission === "granted") {
            myResolve(Notification.permission)
        } else if (Notification.permission !== "denied") {
            Notification.requestPermission().then((permission) => {
                myResolve(Notification.permission)
            }).catch((e) => { myResolve(JSON.stringify(e)) });
        }
    })
}

const getSubscription = () => { return new Promise(function(myResolve, myReject) {
    registerWorker().then(
        (registration) => getSubscriptionFromRegistration(registration).then(
            (subscription) => myResolve(subscriptionToStatus(subscription)),
            (error) => myReject(ERROR_GET_SUB)
        ),
        (error) => myReject(ERROR_GET_REG)
    )
})}

const subscribe = () => { return new Promise(function(myResolve, myReject) {
    registerWorker().then(
        (registration) => subscribeWithRegistration(registration).then(
            (subscription) => myResolve(subscriptionToStatus(subscription)),
            (error) => myReject(ERROR_GET_SUB)
        ),
        (error) => myReject(ERROR_GET_REG)
    )
})}

const unsubscribe = () => { return new Promise(function(myResolve, myReject) {
    registerWorker().then(
        (registration) => subscribeWithRegistration(registration).then(
            (subscription) =>
                subscription.unsubscribe().then(
                   () => myResolve(UNSUBSCRIBED(subscription)),
                   (e) => myReject(ERROR_UNSUB)
                ),
            (error) => myReject(ERROR_GET_SUB)
        ),
        (error) => myReject(ERROR_GET_REG)
    )
})}

const registerWorker = () => { return new Promise(function(myResolve, myReject)  {
    navigator.serviceWorker.register('/sw.js').then(
        (registration) => myResolve(registration),
        (error) => myReject(ERROR_WORKER)
    )
})}

const getSubscriptionFromRegistration = (registration) => {
    return new Promise(function(myResolve, myReject)  {
        registration.pushManager.getSubscription().then(
            (subscription) => myResolve(subscription),
            (error) => myReject(ERROR_GET_SUB)
        )
    })
}

const subscriptionToStatus = (sub) => {
     if (!(sub === null)) {
        return SUBSCRIBED(sub)
     } else {
         return UNSUBSCRIBED(sub)
     }
}

const subscribeWithRegistration = (registration) => {
    return new Promise(function(myResolve, myReject) {
        registration.pushManager.subscribe({
            userVisibleOnly: true,
            applicationServerKey: applicationServerKeyEncoded
        })
        .then(
            (subscription) => myResolve(subscription),
            (error) => myReject(ERROR_SUB)
        )
    });
};

export const PushApp = {
    getPermission: getPermission,
    requestPermission: requestPermission,
    getSubscription: getSubscription,
    subscribe: subscribe,
    unsubscribe: unsubscribe,
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