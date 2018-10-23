var authorName = 'Jesper Dangaard Brouer';
var authorName2 = 'Toke Høiland-Jørgensen';
var authorEmail = 'brouer@redhat.com';
var authorEmail2 = 'toke@toke.dk';

var authorHTML = [authorName, ' &lt;', authorEmail, '&gt; &amp;&nbsp;',
                  authorName2, ' &lt;', authorEmail2, '&gt; &nbsp;',
                  '<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">',
                  '<i class="icon-cc-1"></i>',
                  '<i class="icon-cc-by"></i>',
                  '<i class="icon-cc-sa"></i>',
                  '</a>'].join('');

function addAuthor(selector) {
    return function() {
        var elems = document.querySelectorAll(selector);
        elems.forEach(function (e) {
            var author = document.createElement('div');
            author.className = 'authorbox';
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
    window.addEventListener('load', addAuthor('.reveal .slides > section > section'));
}
window.addEventListener('DOMContentLoaded', hideFake('section'));
