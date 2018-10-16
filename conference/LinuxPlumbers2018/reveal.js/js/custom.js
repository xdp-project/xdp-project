var authorName = 'Jesper Dangaard Brouer';
var authorName2 = 'Toke Høiland-Jørgensen';
var authorEmail = 'brouer@redhat.com';
var authorEmail2 = 'toke@toke.dk';

var authorHTML = [authorName, ' &lt;', authorEmail, '&gt; &amp;&nbsp;',
                  authorName2, ' &lt;', authorEmail2, '&gt; &nbsp;',
                  '<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">',
                  '<img style="border-width: 0px;" alt="License: CC BY-SA" src="./reveal.js/images/cc-by-sa-80x15.png" /></a>'].join('');

function addAuthor(selector) {
    return function() {
        var elems = document.querySelectorAll(selector);
        elems.forEach(function (e) {
            var author = document.createElement('div');
            author.className = 'author';
            author.innerHTML = authorHTML;
            e.appendChild(author);
        });
    }
}

function hideFake(selector) {
    return function() {
        var elems = document.querySelectorAll(selector);
        elems.forEach(function (e) {
            console.log(e.getAttribute('fake'));
            if (e.getAttribute('fake')) {
                e.parentElement.removeChild(e);
            }
        });
    }
}


if (window.location.search.match( /print-pdf/gi )) {
    window.addEventListener('pdf-ready', hideFake('.pdf-page'));
} else {
    window.addEventListener('load', addAuthor('.reveal'));
}
window.addEventListener('DOMContentLoaded', hideFake('section'));
