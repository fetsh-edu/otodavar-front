self.addEventListener('push', function(event) {
  console.log('[Service Worker] Push Received.');
  console.log("[Service Worker] Push had this data:", event.data);
    // TODO: Filter logged out by bearer in localhost

  const title = 'oto | davar';
  const options = {
    body: JSON.parse(event.data.text()).body,
    icon: 'android-chrome-192x192.png',
    badge: 'favicon-16x16.png',
    data: {
        url: JSON.parse(event.data.text()).url
    }
  };


    event.waitUntil(self.registration.showNotification(title, options));
//    event.waitUntil(
//        clients.matchAll({ type: "window"})
//            .then(
//                (clientList) => {
//                    for (const client of clientList) {
//                        if (client.visibilityState === "visible") {
//                            return true;
//                        }
//                    }
//                    return false;
//                }
//            ).then(
//                (visible) => {
//                    if (visible) {
//                    } else {
//                        self.registration.showNotification(title, options)
//                    }
//                }
//            )
//    )
})

self.addEventListener('notificationclick', function(event) {
    console.log('[Service Worker] Notification click Received.');
    console.log("event", event.data)
    event.notification.close();

    let url = event.notification.data.url || "/"

    event.waitUntil(clients.matchAll({
        type: "window"
      }).then((clientList) => {
        for (const client of clientList) {
            console.log("URLS", client.url, url)
          if ((new URL(client.url)).pathname === url && 'focus' in client)
            return client.focus();
        }
        if (clients.openWindow)
          return clients.openWindow(url);
      }));
});