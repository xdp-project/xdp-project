var authorName = 'Jesper Dangaard Brouer';
var authorName2 = 'Andy Gospodarek';
var authorEmail = 'brouer@redhat.com';
var authorEmail2 = 'gospo@broadcom.com';

var authorHTML = [
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
    }
}

/* needs to be in an onLoad callback, otherwise hljs is not defined yet */
function addHighlightKeywords() {
        hljs.getLanguage('C').k.keyword += ' XDP_DROP XDP_PASS XDP_ABORTED XDP_REDIRECT XDP_TX';
        hljs.getLanguage('C').k.keyword += ' data data_end';
}

window.addEventListener('load', addAuthor('.reveal .slides > section > section'));
window.addEventListener('load', addAuthor('#sec-title-slide'));
window.addEventListener('load', addHighlightKeywords);
