var authorName = 'Jesper Dangaard Brouer';
var authorName2 = 'Andy Gospodarek';
var authorEmail = 'brouer@redhat.com';
var authorEmail2 = 'andy@greyhouse.net';

var authorHTML = [
    '<img src="./reveal.js/images/RedHat.svg"/>',
    '<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">',
    '<i class="icon-cc-1"></i>',
    '<i class="icon-cc-by"></i>',
    '<i class="icon-cc-sa"></i>',
    '</a><span class="authors"> - ',
    authorName, ' &lt;', authorEmail, '&gt; &amp;&nbsp;',
    authorName2, ' &lt;', authorEmail2, '&gt; &nbsp;</span>'].join('');

function addAuthor(selector) {
    return function() {
        var elems = document.querySelectorAll(selector);
        elems.forEach(function (e) {
            var author = document.createElement('div');
            author.className = 'authorbox';
            author.innerHTML = authorHTML;
            e.appendChild(author);
        });

	// UGLY HACK!!! - please fix - extra keywords for XDP-code
	hljs.getLanguage('C').k += ' XDP_DROP XDP_PASS XDP_ABORTED XDP_REDIRECT';
	hljs.getLanguage('C').k += ' data data_end';
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
    window.addEventListener('load', addAuthor('#sec-title-slide'));
}
window.addEventListener('DOMContentLoaded', hideFake('section'));
