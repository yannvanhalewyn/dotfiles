Dragon fly
==========

1 - Add gem
-----------

2 - generate model
------------------
    rails g scaffold dragonfly_image --skip-assets

3 - generate dragonfly
----------------------
    rails g dragonfly

*this generates the initialiser file*

4 - update the migration to store info about the caching
--------------------------------------------------------
    t.string :asset_uid
    t.string :asset_name

5 - add accessor to model
-------------------------
in `dragonfly_image.rb`

    class DragonflyImage < ActiveRecor::Base
      dragonfly_accessor :asset
    end

6 - migrate DB
--------------

7 - Forms
---------
Same as the two others

8 - Permit assets
-----------------
    params.require(:dragonfly_image).permit(:asset)

9 - Linking to it
-----------------
    <%= image_tag @dragonfly_image.asset.thumb('300x300').url, class: xx if @dragonfly_image.asset_stored? %>

10 - In the form, display it if exists
--------------------------------------
    <% if f.object.asset_stored? %>
      ..
    <% end %>
