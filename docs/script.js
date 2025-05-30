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
      articleTitle.innerHTML = `<a href="${article.url}" target="_blank">${article.title}</a>`;
      articleDiv.appendChild(articleTitle);

      const authors = document.createElement('div');
      authors.className = 'article-authors';
      authors.textContent = article.authors;
      articleDiv.appendChild(authors);

      if (article.abstract) {
        const abstract = document.createElement('div');
        abstract.className = 'article-abstract';
        abstract.textContent = article.abstract;
        articleDiv.appendChild(abstract);
      }

      journalDiv.appendChild(articleDiv);
    });

    container.appendChild(journalDiv);
  });
}

loadArticles();
