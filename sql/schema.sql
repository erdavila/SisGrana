CREATE TABLE IF NOT EXISTS asset_change(
    asset TEXT NOT NULL,
    stockbroker TEXT NOT NULL,
    date TEXT NOT NULL,

    previous_position_quantity INTEGER NOT NULL,
    previous_position_average_price NUMERIC NOT NULL,
    previous_position_average_cost NUMERIC NOT NULL,

    event_effect_type TEXT NULL,

    event_set_position_quantity INTEGER NOT NULL,
    event_set_position_average_price NUMERIC NOT NULL,
    event_set_position_average_cost NUMERIC NOT NULL,

    event_increase_quantity INTEGER NOT NULL,
    event_increase_average_price NUMERIC NOT NULL,
    event_increase_average_cost NUMERIC NOT NULL,

    event_decrease_quantity INTEGER NOT NULL,
    event_decrease_average_price NUMERIC NOT NULL,
    event_decrease_average_cost NUMERIC NOT NULL,

    purchase_quantity INTEGER NOT NULL,
    purchase_average_price NUMERIC NOT NULL,
    purchase_average_cost NUMERIC NOT NULL,

    sale_quantity INTEGER NOT NULL,
    sale_average_price NUMERIC NOT NULL,
    sale_average_cost NUMERIC NOT NULL,

    exercised_quantity NUMERIC NOT NULL,

    resulting_position_quantity INTEGER NOT NULL,
    resulting_position_average_price NUMERIC NOT NULL,
    resulting_position_average_cost NUMERIC NOT NULL,

    PRIMARY KEY (date, asset, stockbroker)
);

CREATE TABLE IF NOT EXISTS asset_quote(
    asset TEXT NOT NULL,
    date TEXT NOT NULL,

    open_price NUMERIC NOT NULL,
    close_price NUMERIC NOT NULL,
    min_price NUMERIC NOT NULL,
    avg_price NUMERIC NOT NULL,
    max_price NUMERIC NOT NULL,

    PRIMARY KEY (asset, date)
);
