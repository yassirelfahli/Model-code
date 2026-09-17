* =========================================================================
* DYNAMIC FORESTRY EQUILIBRIUM MODEL - FULL MCP/KKT VERSION
* =========================================================================

SETS
    r       "Regions" /TroutLake, Abitibi, Algoma, Kenora, Nipissing, Algonquin/
    p       "Products" /CLT, Glulam, Lumber, Pulpwood/
    y       "Years" /2020*2030/
    tree    "Tree Species" /Spruce, JackPine, WhiteBirch, TolerantHdwds, WhitePine, RedPine, Aspen/
    s       "Site Class" /SC1, SC2, SC3/
    age     "Forest Age" /0*150/
    m       "Manufacturing layers" /Primary, Secondary/;

ALIAS (r, rr);
ALIAS (y, yy);

* =========================================================================
* 1. DATA INPUT: YIELD TABLES
* =========================================================================
PARAMETER YieldData(tree, s, age) "Gross Merchantable Volume (m3 per hectare)";

YieldData('WhitePine', 'SC1', '30') = 76;   YieldData('WhitePine', 'SC1', '40') = 157;
YieldData('WhitePine', 'SC1', '50') = 242;  YieldData('WhitePine', 'SC1', '60') = 315;
YieldData('WhitePine', 'SC1', '70') = 372;  YieldData('WhitePine', 'SC1', '80') = 414;

YieldData('JackPine', 'SC1', '30') = 89;    YieldData('JackPine', 'SC1', '40') = 155;
YieldData('JackPine', 'SC1', '50') = 196;   YieldData('JackPine', 'SC1', '60') = 223;
YieldData('JackPine', 'SC1', '70') = 238;   YieldData('JackPine', 'SC1', '80') = 243;

YieldData('TolerantHdwds', 'SC1', '30') = 36;
YieldData('TolerantHdwds', 'SC1', '40') = 68;
YieldData('TolerantHdwds', 'SC1', '50') = 100;
YieldData('TolerantHdwds', 'SC1', '60') = 130;
YieldData('TolerantHdwds', 'SC1', '70') = 158;
YieldData('TolerantHdwds', 'SC1', '80') = 185;

YieldData(tree, 'SC2', age) = YieldData(tree, 'SC1', age) * 0.80;
YieldData(tree, 'SC3', age) = YieldData(tree, 'SC1', age) * 0.60;

YieldData('RedPine', s, age)    = YieldData('WhitePine', s, age);
YieldData('Spruce', s, age)     = YieldData('JackPine', s, age);
YieldData('Aspen', s, age)      = YieldData('JackPine', s, age) * 0.9;
YieldData('WhiteBirch', s, age) = YieldData('JackPine', s, age) * 0.8;

* =========================================================================
* 2. REGIONAL PARAMETERS
* =========================================================================
TABLE speciesMix(r, tree) "Forest Composition (%)"
               Spruce  JackPine WhiteBirch TolerantHdwds WhitePine RedPine Aspen
    TroutLake   0.50    0.30     0.10       0.00          0.00      0.00    0.10
    Abitibi     0.60    0.20     0.10       0.00          0.00      0.00    0.10
    Kenora      0.40    0.40     0.10       0.00          0.00      0.05    0.05
    Algoma      0.10    0.05     0.10       0.40          0.30      0.05    0.00
    Nipissing   0.10    0.05     0.10       0.30          0.40      0.05    0.00
    Algonquin   0.05    0.00     0.10       0.50          0.30      0.05    0.00;

PARAMETERS
    initialAge(r)         /TroutLake 30, Abitibi 30, Algoma 30, Kenora 30, Nipissing 30, Algonquin 30/
    initialStockRegion(r) /TroutLake 28999.48, Abitibi 107775.22, Algoma 54884.89, Kenora 49746.19, Nipissing 45261.69, Algonquin 36725.26/;

SET regionSite(r, s) "Mapping each region to its dominant site class" /
    TroutLake.SC2
    Abitibi.SC2
    Algoma.SC1
    Kenora.SC3
    Nipissing.SC1
    Algonquin.SC1
/;

PARAMETERS
    regionArea(r)      "Calculated hectares"
    CAI_Region(r, y)   "Annual increment by region (million m3/year)"
    allocShare(p)      "Initial stock allocation shares"
    initialStock(r,p)  "Initial stock by product (million m3)";

regionArea(r) =
    (initialStockRegion(r) * 1000000) /
    sum((tree, age, s)$(ord(age)-1 = initialAge(r) and regionSite(r, s)),
        speciesMix(r, tree) * YieldData(tree, s, age));

LOOP(y,
   LOOP(r,
      CAI_Region(r, y) =
          sum(tree, speciesMix(r, tree) *
             (
               sum((age, s)$(ord(age)-1 = initialAge(r) + 10 and regionSite(r, s)), YieldData(tree, s, age))
             - sum((age, s)$(ord(age)-1 = initialAge(r)      and regionSite(r, s)), YieldData(tree, s, age))
             )
          ) / 10 * regionArea(r) / 1000000;
   );
);

allocShare(p) = 0;
allocShare('CLT')      = 0.007;
allocShare('Glulam')   = 0.003;
allocShare('Lumber')   = 0.65;
allocShare('Pulpwood') = 0.15;

initialStock(r,p) = allocShare(p) * initialStockRegion(r);

* =========================================================================
* 3. ECONOMIC PARAMETERS
* =========================================================================
SCALAR carbonPrice "Carbon price ($/tCO2)" /150/;

PARAMETERS
    basePrice(p)             "Base price ($/m3)"
    yieldFactor(p)           "Production yield"
    manufacturingTier(p)     "1 primary / 2 secondary"
    maxHarvestRate(r)        "Max harvest share of available stock"
    lumberInputPerUnit(p)    "Lumber needed per unit of product"
    growthRate(r)            "SAME - not used in KKT version"
    baseProcessingCost(m)    "Base processing cost by tier ($/m3)"
    productMarkup(p)         "Extra processing markup ($/m3)"
    coreProcessingCost(p)    "Core processing cost excluding lumber feedstock ($/m3)"
    processingCost_y(p,y)    "Processing cost by year ($/m3)"
    basePrice_y(p,y)         "Time-varying base price ($/m3)"
    baseDemand(r,p)          "Base demand (million m3)"
    targetElas(p)            "Price elasticity at base point"
    demandSlope(r,p)         "Direct demand slope dQ/dP"
    transportCost(r,rr,p)    "Transport cost ($/m3)"
    costHarvest(p)           "Harvest cost ($/m3 harvested)"
    carbonEmissionFactor(p)  "Production emission factor (tCO2/m3)"
    carbonStorageFactor(p)   "Stored carbon factor"
    transportEmissionFactor(r,rr,p) "Transport emissions (tCO2/m3)"
    stockFloorMin(r,p,y)     "Minimum retained stock (million m3)"
    invDemIntercept(r,p,y)   "Intercept term for inverse demand";

basePrice(p) = 0;
basePrice('CLT')      = 700;
basePrice('Glulam')   = 575;
basePrice('Lumber')   = 375;
basePrice('Pulpwood') = 75;

yieldFactor(p) = 0;
yieldFactor('CLT')      = 0.82;
yieldFactor('Glulam')   = 0.85;
yieldFactor('Lumber')   = 0.90;
yieldFactor('Pulpwood') = 0.98;

manufacturingTier(p) = 0;
manufacturingTier('CLT')      = 2;
manufacturingTier('Glulam')   = 2;
manufacturingTier('Lumber')   = 1;
manufacturingTier('Pulpwood') = 1;

maxHarvestRate(r) = 0.01;

lumberInputPerUnit(p) = 0;
lumberInputPerUnit('CLT')    = 1.2;
lumberInputPerUnit('Glulam') = 1.1;
lumberInputPerUnit('Lumber') = 1.0;
lumberInputPerUnit('Pulpwood') = 0.0;

growthRate(r) = 0.01;

baseProcessingCost(m) = 0;
baseProcessingCost('Primary')   = 100;
baseProcessingCost('Secondary') = 50;

productMarkup(p) = 0;
productMarkup('CLT')      = 150;
productMarkup('Glulam')   = 120;
productMarkup('Lumber')   = 20;
productMarkup('Pulpwood') = -80;

coreProcessingCost(p) =
    (baseProcessingCost('Primary')   + productMarkup(p))$(manufacturingTier(p)=1)
  + (baseProcessingCost('Secondary') + productMarkup(p))$(manufacturingTier(p)=2);

basePrice_y(p,y) = basePrice(p) * power(1.03, ord(y)-1);

processingCost_y(p,y) =
      coreProcessingCost(p) * power(0.98, ord(y)-1)
    + basePrice_y('Lumber',y) * lumberInputPerUnit(p)$(not sameas(p,'Lumber') and lumberInputPerUnit(p)>0);

baseDemand(r,p) = 0;
baseDemand('Nipissing','Lumber')  = 6.5;
baseDemand('Abitibi','Lumber')    = 5.0;
baseDemand('Algoma','Lumber')     = 4.5;
baseDemand('Algonquin','Lumber')  = 4.0;
baseDemand('TroutLake','Lumber')  = 3.5;
baseDemand('Kenora','Lumber')     = 2.5;

baseDemand('Nipissing','Pulpwood') = 7.0;
baseDemand('Abitibi','Pulpwood')   = 6.0;
baseDemand('Algoma','Pulpwood')    = 5.0;
baseDemand('Algonquin','Pulpwood') = 4.0;
baseDemand('TroutLake','Pulpwood') = 3.0;
baseDemand('Kenora','Pulpwood')    = 2.0;

baseDemand(r,'CLT')    = 0.17;
baseDemand(r,'Glulam') = 0.17;

targetElas(p) = 0;
targetElas('CLT')      = -0.20;
targetElas('Glulam')   = -0.45;
targetElas('Lumber')   = -1.10;
targetElas('Pulpwood') = -0.08;

demandSlope(r,p) = targetElas(p) * baseDemand(r,p) / basePrice(p);

TABLE transportBase(r,rr)
                TroutLake Abitibi Algoma Kenora Nipissing Algonquin
    TroutLake       0        38      42     100      14         9
    Abitibi        26         0      35      75      25        32
    Algoma         31        28       0      60      31        38
    Kenora         79        61      49       0      79        87
    Nipissing      14        27      31      79       0         8
    Algonquin       9        34      38      87       8         0;

transportCost(r,rr,p) = 0;
transportCost(r,rr,p)$(not sameas(r,rr)) = transportBase(r,rr);

costHarvest(p) = 0;
costHarvest('CLT')      = 40;
costHarvest('Glulam')   = 42;
costHarvest('Lumber')   = 38;
costHarvest('Pulpwood') = 30;

carbonEmissionFactor(p) = 0;
carbonEmissionFactor('CLT')      = 0.15;
carbonEmissionFactor('Glulam')   = 0.17;
carbonEmissionFactor('Lumber')   = 0.065;
carbonEmissionFactor('Pulpwood') = 0.01;

carbonStorageFactor(p) = 0;
carbonStorageFactor('CLT')      = 0.9;
carbonStorageFactor('Glulam')   = 0.9;
carbonStorageFactor('Lumber')   = 0.6;
carbonStorageFactor('Pulpwood') = 0.1;

transportEmissionFactor(r,rr,p) = 0;
transportEmissionFactor(r,rr,p)$(not sameas(r,rr)) = 0.018;

stockFloorMin(r,p,y) = 0;
stockFloorMin(r,p,y)$(ord(y) >= 6) = 0.5 * initialStock(r,p);

invDemIntercept(r,p,y) = basePrice_y(p,y) - baseDemand(r,p) / demandSlope(r,p);

* =========================================================================
* 4. PRIMAL VARIABLES
* =========================================================================
POSITIVE VARIABLES
    stock(r,p,y)        "Stock at end of period (million m3)"
    harvest(r,p,y)      "Harvest volume (million m3)"
    production(r,p,y)   "Production volume (million m3)"
    demand(r,p,y)       "Demand volume (million m3)"
    exports(r,rr,p,y)   "Trade flow (million m3)";

* =========================================================================
* 5. DUAL VARIABLES / SHADOW PRICES
* =========================================================================
FREE VARIABLES
    rho(r,p,y)          "Shadow value of stock balance"
    nu(r,p,y)           "Shadow value of production identity"
    price(r,p,y)        "Market clearing shadow price";

POSITIVE VARIABLES
    pi(r,p,y)           "Multiplier on harvest sustainability"
    phi(r,p,y)          "Multiplier on minimum stock floor";

* =========================================================================
* 6. PRIMAL FEASIBILITY EQUATIONS
* =========================================================================
EQUATIONS
    stockBal(r,p,y)     "Dynamic stock balance"
    prodLink(r,p,y)     "Production-harvest linkage"
    marketClr(r,p,y)    "Market clearing"
    sustain(r,p,y)      "Harvest cannot exceed sustainable share"
    stockFloorEq(r,p,y) "Minimum stock retention"

* stationarity equations
    statStock(r,p,y)
    statHarvest(r,p,y)
    statProd(r,p,y)
    statDemand(r,p,y)
    statTrade(r,rr,p,y);

stockBal(r,p,y)..
    stock(r,p,y)
    - ( initialStock(r,p) - harvest(r,p,y) )$(ord(y)=1)
    - ( stock(r,p,y-1) + CAI_Region(r,y) * allocShare(p) - harvest(r,p,y) )$(ord(y)>1)
    =E= 0;

prodLink(r,p,y)..
    production(r,p,y) - yieldFactor(p) * harvest(r,p,y) =E= 0;

marketClr(r,p,y)..
    production(r,p,y) + sum(rr, exports(rr,r,p,y))
    - demand(r,p,y)   - sum(rr, exports(r,rr,p,y))
    =E= 0;

sustain(r,p,y)..
      maxHarvestRate(r) * initialStock(r,p)$(ord(y)=1)
    + maxHarvestRate(r) * stock(r,p,y-1)$(ord(y)>1)
    - harvest(r,p,y)
    =G= 0;

stockFloorEq(r,p,y)..
    stock(r,p,y) - stockFloorMin(r,p,y) =G= 0;

* =========================================================================
* 7. STATIONARITY CONDITIONS (KKT)
* =========================================================================

* Stock stationarity:
* Current rho
* minus next-period rho from stock appearing in next stock balance
* plus next-period harvest-cap rent through sustain
* plus current stock-floor rent
statStock(r,p,y)..
      rho(r,p,y)
    - rho(r,p,y+1)$(ord(y) < card(y))
    + maxHarvestRate(r) * pi(r,p,y+1)$(ord(y) < card(y))
    + phi(r,p,y)
    =G= 0;

* Harvest stationarity:
* harvest cost
* + effect on stock balance
* - yield * production-link shadow
* + current scarcity rent on harvest cap
statHarvest(r,p,y)..
      costHarvest(p)
    + rho(r,p,y)
    - yieldFactor(p) * nu(r,p,y)
    + pi(r,p,y)
    =G= 0;

* Production stationarity:
* processing + carbon cost
* + prod-link shadow
* - market price
statProd(r,p,y)..
      processingCost_y(p,y)
    + carbonPrice * carbonEmissionFactor(p)
    + nu(r,p,y)
    - price(r,p,y)
    =G= 0;

* Demand stationarity:
* market price >= marginal willingness to pay
statDemand(r,p,y)..
    price(r,p,y) - ( basePrice_y(p,y) + (demand(r,p,y) - baseDemand(r,p)) / demandSlope(r,p) )
    =G= 0;

* Trade stationarity:
* origin price + transport + carbon freight >= destination price
statTrade(r,rr,p,y)$(not sameas(r,rr))..
      price(r,p,y)
    + transportCost(r,rr,p)
    + carbonPrice * transportEmissionFactor(r,rr,p)
    - price(rr,p,y)
    =G= 0;

* =========================================================================
* 8. BOUNDS / STARTING VALUES
* =========================================================================
stock.lo(r,p,y)      = 0;
harvest.lo(r,p,y)    = 0;
production.lo(r,p,y) = 0;
demand.lo(r,p,y)     = 0;
exports.lo(r,rr,p,y) = 0;
price.lo(r,p,y)      = 0;

stock.l(r,p,y)      = initialStock(r,p);
harvest.l(r,p,y)    = maxHarvestRate(r) * initialStock(r,p) * 0.5;
production.l(r,p,y) = yieldFactor(p) * harvest.l(r,p,y);
demand.l(r,p,y)     = baseDemand(r,p);
exports.l(r,rr,p,y) = 0;
price.l(r,p,y)      = basePrice_y(p,y);

rho.l(r,p,y) = 0;
nu.l(r,p,y)  = 0;
pi.l(r,p,y)  = 0.01;
phi.l(r,p,y) = 0.01;

* =========================================================================
* 9. MODEL DECLARATION
* =========================================================================
MODEL ForestKKT /
    stockBal.rho
    prodLink.nu
    marketClr.price
    sustain.pi
    stockFloorEq.phi

    statStock.stock
    statHarvest.harvest
    statProd.production
    statDemand.demand
    statTrade.exports
/;

SOLVE ForestKKT USING MCP;

* =========================================================================
* 10. REPORTING
* =========================================================================
PARAMETERS
    carbonStorageRep(r,p,y)  "Carbon storage"
    carbonEmissionRep(r,p,y) "Carbon emissions (million tCO2)"
    revRPY(r,p,y)            "Revenue (million $)"
    revR(r)                  "Revenue by region (million $)"
    rev2025(r)               "Revenue in 2025 (million $)"
    tierMetrics(r,m,*,y)     "Manufacturing tier metrics"
    productChainCost(p,*,y)  "Cost breakdown"
    revenue                  "Total revenue (million $)"
    harvestCostTot           "Total harvest cost (million $)"
    procCostTot              "Total processing cost (million $)"
    transCostTot             "Total transport cost (million $)"
    Zemission                "Total emissions (million tCO2)"
    carbonCost               "Carbon cost (million $)"
    welfare                  "Gross welfare objective (million $)"
    producerNet              "Revenue net of private costs (million $)"
    obj                      "Net after carbon cost (million $)";

carbonStorageRep(r,p,y) =
    carbonStorageFactor(p) * production.l(r,p,y);

carbonEmissionRep(r,p,y) =
      carbonEmissionFactor(p) * production.l(r,p,y)
    + sum(rr, transportEmissionFactor(r,rr,p) * exports.l(r,rr,p,y));

revRPY(r,p,y) = price.l(r,p,y) * demand.l(r,p,y);
revR(r)       = sum((p,y), revRPY(r,p,y));
rev2025(r)    = sum(p, revRPY(r,p,'2025'));

tierMetrics(r,m,'Production',y) =
    sum(p$(manufacturingTier(p)=ord(m)), production.l(r,p,y));

tierMetrics(r,m,'Revenue',y) =
    sum(p$(manufacturingTier(p)=ord(m)), price.l(r,p,y) * demand.l(r,p,y));

tierMetrics(r,m,'ProcessingCost',y) =
    sum(p$(manufacturingTier(p)=ord(m)), processingCost_y(p,y) * production.l(r,p,y));

productChainCost(p,'BaseProcessing',y) =
    sum(m$(manufacturingTier(p)=ord(m)), baseProcessingCost(m));

productChainCost(p,'ProductMarkup',y) = productMarkup(p);

productChainCost(p,'LumberInputCost',y) =
    basePrice_y('Lumber',y) * lumberInputPerUnit(p)$(not sameas(p,'Lumber'));

productChainCost(p,'Total',y) = processingCost_y(p,y);

revenue =
    sum((r,p,y), price.l(r,p,y) * demand.l(r,p,y));

harvestCostTot =
    sum((r,p,y), costHarvest(p) * harvest.l(r,p,y));

procCostTot =
    sum((r,p,y), processingCost_y(p,y) * production.l(r,p,y));

transCostTot =
    sum((r,rr,p,y), transportCost(r,rr,p) * exports.l(r,rr,p,y));

Zemission =
    sum((r,p,y), carbonEmissionRep(r,p,y));

carbonCost =
    carbonPrice * Zemission;

* Gross welfare from integrating inverse demand
* consumer surplus + producer surplus - costs.
welfare =
    sum((r,p,y),
          basePrice_y(p,y) * demand.l(r,p,y)
        + ( sqr(demand.l(r,p,y)) - 2 * baseDemand(r,p) * demand.l(r,p,y) ) / (2 * demandSlope(r,p))
    )
    - harvestCostTot
    - procCostTot
    - transCostTot
    - carbonCost;

producerNet = revenue - harvestCostTot - procCostTot - transCostTot;

obj = producerNet - carbonCost;

DISPLAY price.l, demand.l, production.l, harvest.l, stock.l, exports.l,
        rho.l, nu.l, pi.l, phi.l,
        revenue, producerNet , welfare, obj,
        harvestCostTot, procCostTot, transCostTot,
        Zemission, carbonCost,
        revR, rev2025, tierMetrics, productChainCost,
        CAI_Region, regionArea, speciesMix;

EXECUTE_UNLOAD "ForestKKTResults.gdx",
    price, demand, production, harvest, stock, exports,
    rho, nu, pi, phi,
    revenue, producerNet, welfare, obj, Zemission, carbonCost,
    revR, rev2025, tierMetrics;

EXECUTE "gdx2csv ForestKKTResults.gdx";

* =========================================================================
* 11. Verifing
* =========================================================================




