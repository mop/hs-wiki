<html>
  <head>
    <title>HS-Wiki | A wiki using HAppS and MACID</title>
    <link rel="stylesheet" href="/stylesheets/default.css" type="text/css" />
  </head>
  <body>
    <div id="menu">
      <ul>
        <li class="active">
          <a href="#">Article</a>
        </li>
        <li>
          <a href="#">History</a>
        </li>
        <li>
          <a href="#">Edit</a>
        </li>
      </ul>
    </div>
    <div id="login">
        <ul>
            <li>
                <a href="/login">Login</a> 
            </li>
            <li>
                <a href="/register">Register</a>
            </li>
        </ul>
    </div>
    <div id="content">
      $content$
    </div>
    <div id="logo"><img src="/images/logo.png" alt="logo" /></div>
    <div id="search">
      <i>Search</i>
      <form method="post" action="/search">
        <input type="text" name="search_query" /> 
        <input type="submit" value="Search" />
      </form>
    </div>
    <div id="navigation">
      <i>Navigation</i>
      <ul>
        <li>
          <a href="/">Home</a>
        </li>
        <li>
          <a href="/about">About</a>
        </li>
        <li>
          <a href="/categories">All Categories</a>
        </li>
        <li>
          <a href="/articles">All Articles</a>
        </li>
        <li>
          <a href="/articles/random">Random Article</a>
        </li>
      </ul>
    </div>
    <div id="footer">
      hs-wiki &copy; Michael Opitz
    </div>
  </body>
</html>
