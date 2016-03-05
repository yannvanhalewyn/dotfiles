
Steps to setup user authentication via Facebook!
================================================



1 - Add gems and run bundle
---------------------------
    gem 'omniauth'
    gem 'omniauth-facebook', '~> x'


2 - Create the user model
-------------------------

    rails g model User provider uid name oauth_token {first_name, thumb, ..}
      oauth_expires_at:datetime


3 - Create the method in the user model to create a user using omniauth
-----------------------------------------------------------------------

    class User < ActiveRecord::Base
      def self.from_omniauth(auth)
        where(provider: auth.provider, uid: auth.uid).first_or_create do |user|
          user.provider = auth.provider
          user.uid = auth.uid
          user.name = auth.info.name
          user.first_name = auth.info.first_name
          user.last_name = auth.info.last_name
          user.email = auth.info.email
          user.image = auth.info.image
          user.fb_profile_url = auth.info.urls.Facebook
          user.oauth_token = auth.credentials.token
          user.oauth_expires_at = Time.at(auth.credentials.expires_at)
        end
      end
    end


4 - Create the current_user helper in application controller
------------------------------------------------------------

    class ApplicationController < ActionController::Base
      protect_from_forgery with: :exception

      private
      def current_user
        @current_user ||= User.find(session[:user_id]) if session[:user_id]
      end
      helper_method :current_user
    end


5 - Create sessions controller
------------------------------

    class SessionsController < ApplicationController
      def create
        user = User.from_omniauth(env["omniauth.auth"])
        session[:user_id] = user.id
        redirect_to root_url
      end

      def destroy
        session[:user_id] = nil
        redirect_to root_url
      end
    end
**NOTE: You might want to use request.env["omniauth.auth"]. evn[] works, but coulnd't get the tests working accordingly


6 - Setup routes
----------------

    match 'auth/:provider/callback', to: 'sessions#create', via: [:get, :post]
    match 'auth/failure', to: redirect('/'), via: [:get, :post]
    match 'signout', to: 'sessions#destroy', as: 'signout', via: [:get, :post]


7 - Create omniauth initializer
--------------------------------

Add this to: `config/initializers/omniauth.rb`

    OmniAuth.config.logger = Rails.logger

    Rails.application.config.middleware.use OmniAuth::Builder do
      provider :facebook, 'APP-ID', 'APP-SECRET'
    end


8 - Add client-side functionality with js
-----------------------------------------

Add this to: `app/assets/javascripts/facebook.js.coffee`

    jQuery ->
      $('body').prepend('<div id="fb-root"></div>')

      $.ajax
        url: "#{window.location.protocol}//connect.facebook.net/en_US/all.js"
        dataType: 'script'
        cache: true

      window.fbAsyncInit = ->
        FB.init(appId: 'APP-ID', cookie: true)

        $('#sign_in').click(e) ->
          e.preventDefault()
          FB.login (response) ->
            window.location = '/auth/facebook/callback' if response.authResponse

        $('#sign_out').click(e) ->
          FB.getLoginStatus (response) ->
            FB.logout() if response.authResponse
          true


9 - An example of a login button link
-------------------------------------
    <div id="user-widget">
      <% if current_user %>
        Welcome <strong><%= current_user.name %></strong>!
        <a href="<%= current_user.fb_profile_url %>">
          <img src="<%= current_user.image %>">
        </a>
        <%= link_to "Sign out", signout_path, id: "sign_out" %>
      <% else %>
        <%= link_to "Sign in with Facebook", "/auth/facebook", id: "sign_in" %>
      <% end %>
    </div>




EXTRA -> Example of the auth hash
=================================


<pre>
{
  "provider": "facebook",
  "uid": "10152783682321643",
  "info": {
    "email": "yann_vanhalewyn@hotmail.com",
    "name": "Yann Vanhalewyn",
    "first_name": "Yann",
    "last_name": "Vanhalewyn",
    "image": "http://graph.facebook.com/10152783682321643/picture",
    "urls": {
      "Facebook": "https://www.facebook.com/app_scoped_user_id/10152783682321643/"
    },
    "verified": true
  },
  "credentials": {
    "token": "CAABfl4VsGAkBAHj14GZARQESFoPeKRpO9fuqvKsJ5g9IbzkXoVmZBtf2ACFcuaz8lrh2zLwUGGA2SA7AWE71W6S0Xt3y1jMsbuOMZBjrrc6MkKkB4kqo6HFijdhqY1K3yG5DLGwkXWL7wRBJMPWwtDka84BmpbAgRENSAKVvA56h6ZCe7RNGh09gQUNqBi1kmMFfXcg2C9lSLtl0PaUS",
    "expires_at": 1435324304,
    "expires": true
  },
  "extra": {
    "raw_info": {
      "id": "10152783682321643",
      "email": "yann_vanhalewyn@hotmail.com",
      "first_name": "Yann",
      "gender": "male",
      "last_name": "Vanhalewyn",
      "link": "https://www.facebook.com/app_scoped_user_id/10152783682321643/",
      "locale": "en_US",
      "name": "Yann Vanhalewyn",
      "timezone": 2,
      "updated_time": "2015-04-27T12:02:51+0000",
      "verified": true
    }
  }
}
</pre>
