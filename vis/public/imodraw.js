function line(x, y, a, b) {
    window.c.b();
    window.c.setLineDash([]);
    window.c.moveTo(x, y);
    window.c.lineTo(a, b);
    window.c.s();
}

function circle(x, y, r) {
    window.c.b();
    window.c.arc(x, y, r, 0, 7, 0);
    window.c.s();
}

function color(cl) {
    window.c.strokeStyle = window.c.fillStyle = cl;
}

function hsl(h, s, l) {
    window.c.strokeStyle = window.c.fillStyle = 'hsl(' + h + ',' + s + ',' + l + ')';
}

function linedash(x, y, a, b) {
    window.c.b();
    window.c.setLineDash([4, 4]);
    window.c.moveTo(x, y);
    window.c.lineTo(a, b);
    window.c.s();
}

function text(str, x, y) {
    if (window.showText) {
        window.c.font = "16px Arial";
        window.c.strokeText(str, x, y);
    }
}

function polyfill(xs, ys) {
    window.c.b();
    window.c.moveTo(xs[0], ys[0]);
    for (var i = 1; i < xs.length; ++i) {
        window.c.lineTo(xs[i], ys[i]);
    }
    window.c.closePath();
    window.c.fill();
}

function animate(frames) {
    var i = 0;
    var timer = window.setInterval(function () {
        window.c.clearRect(0, 0, 1000, 500);
        frames[i++]();
        if (i == frames.length) {
            clearTimeout(timer);
        }
    }, 60);
}

window.onload = function () {
    window.showText = window.location.href.match(/show_text=1/) != null;
    d = document;
    d.i = d.getElementById;
    window.c = d.i('c').getContext('2d');
    window.c.b = window.c.beginPath;
    window.c.s = window.c.stroke;
    document.getElementById('form').action = window.location.pathname;
    document.getElementById('s').src = document.body.getAttribute('data-vis-src');
};
