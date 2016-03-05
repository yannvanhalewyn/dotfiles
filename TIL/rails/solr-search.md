Rails sunspot-solr search
=========================

1. Add gems
-----------

    gem 'sunspot_rails'
    group :development do
        gem 'sunspot_solr'
    end

run `bundle install`


2. Install sunspot
------------------

run `rails generate sunspot_rails:install`

this will create the `/config/sunspot.yml` file

3. Specify the searchable elements in the model
-----------------------------------------------

in `app/models/model.rb`

    searchable do
      text :name, :description
    end

4. Start solr server
--------------------

run `rake sunspot:solr:start`

to stop: `rake sunspot:solr:stop`

5. Update the controller to assign the searched objects
-------------------------------------------------------

in `app/controllers/models_controller.rb`

    def index
      @query = Product.search do
          fulltext params[:search]
      end
      @products = @query.results
    end

Apendix
=======

a. Indexing
-----------

Solr indexes new models as the come in. If you already have entries in the database, or change some configurations, you can run:

`rake sunspot:reindex`

b. Weights
----------

in `model.rb`

    searchable do
      text :name, :boost => 2
      text :description
    end

c. Phrases
----------

    def index
      @query = Product.search do
        fulltext params[:search] do
          phrase_fields name: 2.0
        end
      end
      @products = @query.results
    end
