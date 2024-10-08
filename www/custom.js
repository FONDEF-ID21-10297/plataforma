(function() {
    document.addEventListener("DOMContentLoaded", function() {
        // Código que quieres ejecutar cuando el DOM esté completamente cargado
        console.log("El DOM está completamente cargado.");

        // Selecciona el elemento con el ID específico
        var element = document.getElementById("mainsidebar");

        // Selecciona el contenedor padre del elemento
        var parent = element.parentNode;

        // Mueve el elemento al principio del contenedor
        parent.insertBefore(element, parent.firstChild);

    });
})();