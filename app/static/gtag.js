$(() => {

  /* Default installation */
  window.dataLayer = window.dataLayer || [];
  function gtag() {
    dataLayer.push(arguments);
  }
  gtag('js', new Date());
  gtag('config', 'G-FQQZL5V93G');

  /* App Logo Button */
  $('.logo.logo-main').on('click', (event) => {
    gtag('event', 'Appsilon logo clicked');
    console.log("Appsilon logo clicked")
  });

  /* introduction Tab Button */
  $('#about-info').on('click', (event) => {
    gtag('event', 'info section clicked');
    console.log("info section clicked")
  });

  /* Overview Tab Button */
  $('.logo.logo-dashboard').on('click', (event) => {
    gtag('event', 'App Logo Clicked');
    console.log("App Logo Clicked")
  });

});
