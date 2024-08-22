$(document).ready(() => {
  $('#about-info').click(() => {
    Shiny.setInputValue('app-about-open_modal', 'event', { priority: 'event' });
  });

  /* Default installation */
  window.dataLayer = window.dataLayer || [];
  function gtag(...args) {
    window.dataLayer.push(args);
  }
  gtag('js', new Date());
  gtag('config', 'G-FQQZL5V93G');

  /* App Logo Button */
  $('.logo.logo-main').on('click', () => {
    gtag('event', 'Appsilon logo clicked');
    console.log('Appsilon logo clicked');
  });

  /* introduction Tab Button */
  $('#about-info').on('click', () => {
    gtag('event', 'info section clicked');
    console.log('info section clicked');
  });

  /* Overview Tab Button */
  $('.logo.logo-dashboard').on('click', () => {
    gtag('event', 'App Logo Clicked');
    console.log('App Logo Clicked');
  });
});
