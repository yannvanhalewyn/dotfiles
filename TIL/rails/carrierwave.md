CarrierWave
===========

1 - Add gems (duh)
------------------
    gem 'carrierwave', '~> 0.10.0'
    gem 'mini_magick', '~> 4.2.3'

2 - Initializer thingy
----------------------
    touch config/initializers/carrier_wave.rb
    echo 'require "carrierwave/orm/activerecord"' > config/initializers/carrier_wave.rb

3 - Generate scaffold
---------------------
    rails g scaffold carrierwave_image --skip-assets

4 - Add string for asset in migration
-------------------------------------
    create_table :carrierwave_images do |t|
      t.string :asset
      t.timestamps null: false
    end

5 - Migrate
-----------

6 - Generate uploader
---------------------
    rails g uploader asset

7 - Include MiniMagick in the oploader file
-------------------------------------------
in `app/uploaders/asset_uploader.rb`

    include CarrierWave::MiniMagick

8 - In the uploader file, add the versions you want
---------------------------------------------------
    version :medium do
      process :resize_to_fit => [300, 300]
    end
    version :small do
      process :resize_to_fit => [140, 140]
    end
    version :thumb do
      process :resize_to_fill => [64, 64]
    end

9 - Mount uploader to the model
-------------------------------
in `app/models/carrierwave_image.rb`

    class CarrierwaveImage < ActiveRecord::Base
      mount_uploader :asset, AssetUploader
    end

10 - Add form elements
----------------------
in `app/views/carrierwave_images/_form.html.erb`

    <%= form_for(@carrierwave_image) do |f| %>
      <div class="form-group">
        <%= f.label :asset %>
        <%= f.file_field :asset %>
      </div>

      <div class="actions">
        <%= f.submit "Save", class: "btn btn-primary" %>
      </div>
    <% end %>

11 - Update the params whitelisting
-----------------------------------
in `app/controllers/carrierwave_images_controller.rb`

    def carrierwave_image_params
      params.require(:carrierwave_image).permit(:asset)
    end

12 - Displaying the image:
--------------------------
You call the model instance, the uploader (asset in our case), the type and then the url

    <%= image_tag @carrierwave_image.asset.medium.url %>

EXTRA - Removing the image (checkbox on edit form?)
---------------------------------------------------
1. in the form, within f.object_asst?

      <%= f.label :remove_asset %>
      <%= f.check_box :remove_asset %>

2. Param permission in the image controller

    params.require(:carrierwave_image).permit(:asset, :remove_asset)


