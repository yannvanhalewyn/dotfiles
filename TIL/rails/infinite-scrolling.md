Implementing Infinite Scrolling
===============================

1. Add the pagination gem
-------------------------

    gem 'will_paginate', '~> 3.0.5'

*Optional: for easy button styling in case of no-js*

    gem 'bootstrap-sass', '~> 3.0.3.0'
    gem 'bootstrap-will_paginate', '~> 0.0.10'

in `application.css.scss`

    @import 'bootstrap';

in `app/assets/javascripts/application.js`

    //= require boostrap

`$ Bundle install`

2. Add the pagination data query in the controller
--------------------------------------------------

    @posts = Post.paginate(page: params[:page], per_page: 15).order('created_at DESC')

3. The view (for class and id reference)
----------------------------------------

in `app/views/posts/index.html.erb`

    <div class="page-header">
      <h1>My posts</h1>
    </div>

    <div id="my-posts">
      <%= render @posts %>
    </div>

    <div id="infinite-scrolling">
      <%= will_paginate %>
    </div>

4. The scripting
----------------

create `app/assets/javascripts/pagination.coffee`

We want a script to fire on scroll. It should make a request whenever it's near the bottom of the page (in this case 60px off). When it sends off the request, it should show a little loading gif.

    jQuery ->
      if $('#infinite-scrolling').size() > 0
        $(window).on 'scroll', ->
          more_posts_url = $('.pagination .next_page a').attr('href')
            if more_posts_url && $(window).scrollTop() > $(document).height() - $(window).height() - 60
                $('.pagination').html('<img src="/assets/ajax-loader.gif" alt="Loading..." title="Loading..." />')
                $.getScript more_posts_url
            return
          return

5. Add response to js in controller
-----------------------------------

    @posts = Post.paginate(page: params[:page], per_page: 15).order('created_at DESC')
    respond_to do |format|
      format.html
      format.js
    end

6. Add rendering js file
------------------------

create `app/views/posts/index.js.erb`

The script checks if there is a next page. If none, unbind the scroll action from the window. If there is, rerender the pagination div!

    $('#my-posts').append('<%= j render @posts %>');
    <% if @posts.next_page %>
      $('.pagination').replaceWith('<%= j will_paginate @posts %>');
    <% else %>
      $(window).off('scroll');
      $('.pagination').remove();
    <% end %>
