async function loadArticles() {
  const res = await fetch('./output/jewish_studies.json');
  const data = await res.json();

  document.getElementById('updated').textContent = `Last updated: ${data.update}`;
  const container = document.getElementById('journals');

  data.content.forEach(journal => {
    const journalDiv = document.createElement('div');
    journalDiv.className = 'journal';

    const title = document.createElement('h2');
    title.textContent = journal.journal_full;
    journalDiv.appendChild(title);

    journal.articles.forEach(article => {
      const articleDiv = document.createElement('div');
      articleDiv.className = 'article';

      const articleTitle = document.createElement('div');
      articleTitle.className = 'article-title';

      const linkWrapper = document.createElement('span');
      linkWrapper.className = 'article-link';

      const link = document.createElement('a');
      link.href = article.url;
      link.target = '_blank';
      link.textContent = article.title;

      linkWrapper.appendChild(link);

      if (article.abstract) {
        const tooltip = document.createElement('div');
        tooltip.className = 'abstract-tooltip';
        tooltip.textContent = article.abstract;
        linkWrapper.appendChild(tooltip);
      }

      articleTitle.appendChild(linkWrapper);
      articleDiv.appendChild(articleTitle);

      const authors = document.createElement('div');
      authors.className = 'article-authors';
      authors.textContent = article.authors;
      articleDiv.appendChild(authors);

      journalDiv.appendChild(articleDiv);
    });

    container.appendChild(journalDiv);
  });
}

loadArticles();
