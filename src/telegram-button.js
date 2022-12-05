export default class TelegramButton extends HTMLElement {
  constructor() {
    const self = super();

    self.onauth = (user) => {
        console.log("Inside fire with user", user)
        this.dispatchEvent(new CustomEvent('on-telegram-auth', {detail: user}))
    }

    return self;
  }

  connectedCallback() {
        const script = document.createElement('script');

        script.src = 'https://telegram.org/js/telegram-widget.js?21';
        script.async = true;

        const attributes = {
            'data-telegram-login': this.getAttribute('data-telegram-login'),
            'data-size': this.getAttribute('data-size'),
            'data-radius': this.getAttribute('data-radius'),
            'data-request-access': this.getAttribute('data-request-access'),
            'data-onauth': 'onTelegramAuth(user)',
        };

        for (const [k, v] of Object.entries(attributes)) {
            v !== undefined && script.setAttribute(k, `${v}`);
        }
        this.appendChild(script);
  }
}

const onTelegramAuth = (user) => {
    console.log("data-onauth fired with", user)
    const button = document.querySelector("telegram-button")
    button.onauth(user)
}

if (!window.customElements.get('telegram-button')) {
  window.TelegramButton = TelegramButton
  window.customElements.define('telegram-button', TelegramButton)
  window.onTelegramAuth = onTelegramAuth
}
