# StatShops

Check out the [wiki](https://github.com/CubBossa/StatShops/wiki/Getting-Started) for more information.

## Features

### Shops

- Simply create chest menu based or villager menu based shops
- Add multiple pages for chest based shops (shops will have next/prev page buttons)
- Trade items for items/xp/money (Vault required) and currencies provided by other plugins
- Apply default templates to each shop page
- Set permission constraints for each shop and shop entry
- Colorize the name of shops by using the [Kyori MiniMessage](https://docs.adventure.kyori.net/minimessage#the-components) format
- Create own templates to speed up the process
- Modify settings until the plugin matches your servers style
- Export/Import Shops from other users like Worldedits schematics. Put them in the "presets" folder and import them via command.

### Discounts

- Create discount objects with a start date and a duration
- Constrain discounts to certain users by adding a permission
- Colorize the discount display name
- Add multiple discounts and sum up the discount percentage
- Plan discounts, like e.g. Christmas offers, way before they start
- Add multiple start times per discount to activate it e.g. once every month/weekend

### Limits

- Create limit objects with a global or per user limit of traded items
- Constraint limits to certain users by adding a permission
- Apply multiple limits to shop entries -> it will automatically use the lowest limit
- Reset limits for a user via commands

### Tags

- Group shops and shop entries by applying tags
- Apply limits and discounts to shops or groups of entries with certain tags
- Activate auto tagging, which automatically adds material/potion/enchantment tags for the sold item.

### Templates

- Create custom templates ingame
- Create and register custom templates via API, which allows you to also set items based on the row count (-> item
  always in last row)
- Set a default template for all shop pages of a shop
- View a preview before applying templates

### Logging & Statistics

- All Interactions will be logged into files by the policy provided in the config.yml

### API

- Provide custom currencies
- Provide custom entry types (modules) and costs in case of trade items
- Provide dynamic pricing templates
- Register your own messages to the translation system
