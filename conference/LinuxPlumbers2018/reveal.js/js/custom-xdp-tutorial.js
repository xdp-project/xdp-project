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
	// Disable as it killed other keywords
//	hljs.getLanguage('C').k += ' XDP_DROP XDP_PASS XDP_ABORTED XDP_REDIRECT XDP_TX';
//	hljs.getLanguage('C').k += ' data data_end';
    }
}

window.addEventListener('load', addAuthor('.reveal .slides > section > section'));
window.addEventListener('load', addAuthor('#sec-title-slide'));
