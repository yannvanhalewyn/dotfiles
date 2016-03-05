Paperclip
=========

1 - Add t("fb-profilegeuh)
--------------------------

2 - Setup scaffold for images (--skip-assets)
---------------------------------------------

3 - In the migration, add the attachment AFTER the table. ":asset" is the name of the attachment, can be whatever you like
--------------------------------------------------------------------------------------------------------------------------
     add_attachment :paperclip_images, :asset
*=> This will create 4 columns in the table: (file_name, content_type, file_size & updated_at)*

4 - In the model.rb file, add
-----------------------------
     has_attached_file :asset, styles: {}
          => styles are the different variations of the image that are going to be saved. Example: {
    medium: '300x300>' , // > means it's the max, it won't be bigger than 300 in any dimension
    small: '140x140>',
    thumb: '64x64!' // ! means we demand a forced cropped square image of 64px

5 - Add the validation for "is it an image?"
--------------------------------------------
            validates_attachment_content_type :asset, :content_type => /\Aimage\/.*\Z/

6 - Add the correct stuff in the form_for for it to work
--------------------------------------------------------
    <%= form_for @paperclip_image, html: {multipart: true} do |f| %>
      <%= f.label :asset %>
      <%= f.file_field :asset, html: { class: 'form-control' } %>
      <%= f.submit "Save" %>
    <% end %>

7 - Whitelist the params for POST CREATE/UPDATE
-----------------------------------------------
    def paperclip_image_params
      params.require(:paperclip_image).permit(:asset)
    end

8 - To display the pic somewhere, user an image_tag
---------------------------------------------------
    <%= image_tag @paperclip_image.asset.url(:medium) %>
*(asset is the name we gave the attachment earlier)*
*(:medium is the type we want back)*

EXTRA - In forms, you can display the thumb if it already exists
----------------------------------------------------------------
    <% if f.object.asset? %>
      <br>
      <%= image_tag(f.object.asset.url(:small), class: "img-thumbnail") %>
      <em>Currently uploaded</em>
    <% end %>
