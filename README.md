![Banner](images/banner.png)
# StatShops

Check out the [wiki](https://github.com/CubBossa/StatShops/wiki/Getting-Started) for more information.

## Features

### Shops

- Simply create chest menu based or villager menu based shops
- Add multiple pages for chest based shops (shops will have next/prev page buttons)
- Put all items in a chest and parse the chest to a shop page -> /shops parse-chest <Shop> [<page>]
- Trade articles for items/xp/money (Vault required) and currencies provided by other plugins
- Apply templates (item presets) to each shop page
- Set permission constraints for each shop and shop entry
- Colorize the name of shops by using the [Kyori MiniMessage](https://docs.adventure.kyori.net/minimessage#the-components) format
- Set a custom page name for pages in PaginatedShops
- Create Templates from shop pages to speed up the process
- Customize every message to fit the plugin into your servers style
- Export/Import Shops, Limits, Discounts and Templates as Presets from other users like Worldedits schematic files
  
### Discounts

- Create discount objects with a start date and a duration
- Constrain discounts to certain users by adding a permission
- Colorize and style the discount display name
- Add multiple discounts and sum up the discount percentage
- Plan discounts, like e.g. Christmas offers, way before they start
- Add multiple start times per discount to activate it e.g. once every month/weekend
- Open shop GUIs automatically update if discounts were added or removed or have stared

### Limits

- Create limit objects with a global or per user limit of traded items
- Constraint limits to certain users by adding a permission
- Apply multiple limits to shop entries -> it will automatically use the smallest limit
- Reset limits for a user via commands
- Open shop GUIs automatically update if limits expire

### Tags

- Group shops and shop entries by applying tags
- Apply limits and discounts to shops or groups of entries with certain tags
  -> if a limit and a shop entry (or its according shop) have one tag in common, the limit applies to the entry
- Activate auto tagging, which automatically adds material/potion/enchantment/item group tags for the sold item.

### Templates

- Create custom templates ingame or via API (allows intelligent Templates that stick to the most bottom row etc.)
- Set a default template for all shop pages of a shop
- View a preview before applying templates

### Logging & Statistics

- All Interactions will be logged into files by the policy provided in the config.yml

### API

- Register custom shop types
- Provide custom currencies
- Provide custom entry types (modules) tradable articles and costs
- Provide dynamic pricing templates
- Register your own messages to the translation system
