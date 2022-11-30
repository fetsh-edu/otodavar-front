self.addEventListener('push', function(event) {
  console.log('[Service Worker] Push Received.');
  console.log(`[Service Worker] Push had this data: "${event.data.text()}"`);

  const title = 'Push Codelab';
  const options = {
    body: 'Yay it works.',
    icon: 'favicon-32x32.png',
    badge: 'favicon-16x16.png'
  };

  event.waitUntil(self.registration.showNotification(title, options));
});

self.addEventListener('notificationclick', function(event) {
  console.log('[Service Worker] Notification click Received.');
    console.log("event", event.data)
  event.notification.close();

//  event.waitUntil(
//    clients.openWindow('https://developers.google.com/web/')
//  );
});