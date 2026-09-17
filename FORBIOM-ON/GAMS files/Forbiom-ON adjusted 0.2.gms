* =========================================================================
* FORBIOM MODEL
* - logistics constraints include arc capacities + mill capacities
* - prices are FIXED to exogenous basePrice_y path 
* - all economic flow variables are in MILLION m3
* - prices/costs are in $/m3
* - objective and totals are therefore in MILLION $
* =========================================================================

* =========================================================================
* 1. MODEL STRUCTURE (SETS)
* =========================================================================
SETS
    r       "Regions" /TroutLake, Abitibi, Algoma, Kenora, Nipissing, Algonquin/
    p       "Products" /CLT, Glulam, Lumber, Pulpwood/
    y       "Years" /2020*2030/
    tree    "Tree Species" /Spruce, JackPine, WhiteBirch, TolerantHdwds, WhitePine, RedPine, Aspen/
    s       "Site Class" /SC1, SC2, SC3/
    age     "Forest Age" /0*150/
    m       "Manufacturing layers" /Primary, Secondary/
    met     "Tier metrics" /Production, Revenue, ProcessingCost/
    comp    "Product chain cost components" /BaseProcessing, ProductMarkup, LumberInputCost, Total/;

ALIAS (r, rr);

* =========================================================================
* 2. DATA INPUT: YIELD TABLES (Plonski Volume m3/ha)
* =========================================================================
PARAMETER YieldData(tree, s, age) "Gross Merchantable Volume (m3/ha)";

* [DATA INPUT: WHITE PINE SC1]
YieldData('WhitePine', 'SC1', '30') = 76;   YieldData('WhitePine', 'SC1', '40') = 157;
YieldData('WhitePine', 'SC1', '50') = 242;  YieldData('WhitePine', 'SC1', '60') = 315;
YieldData('WhitePine', 'SC1', '70') = 372;  YieldData('WhitePine', 'SC1', '80') = 414;

* [DATA INPUT: JACK PINE SC1]
YieldData('JackPine', 'SC1', '30') = 89;    YieldData('JackPine', 'SC1', '40') = 155;
YieldData('JackPine', 'SC1', '50') = 196;   YieldData('JackPine', 'SC1', '60') = 223;
YieldData('JackPine', 'SC1', '70') = 238;   YieldData('JackPine', 'SC1', '80') = 243;

* [DATA INPUT: TOLERANT HARDWOODS SC1]
YieldData('TolerantHdwds', 'SC1', '30') = 36;
YieldData('TolerantHdwds', 'SC1', '40') = 68;
YieldData('TolerantHdwds', 'SC1', '50') = 100;
YieldData('TolerantHdwds', 'SC1', '60') = 130;
YieldData('TolerantHdwds', 'SC1', '70') = 158;
YieldData('TolerantHdwds', 'SC1', '80') = 185;

* [DATA INPUT: SITE CLASS PROXIES]
YieldData(tree, 'SC2', age) = YieldData(tree, 'SC1', age) * 0.80;
YieldData(tree, 'SC3', age) = YieldData(tree, 'SC1', age) * 0.60;

* [DATA INPUT: SPECIES PROXIES]
YieldData('RedPine', s, age)    = YieldData('WhitePine', s, age);
YieldData('Spruce', s, age)     = YieldData('JackPine', s, age);
YieldData('Aspen', s, age)      = YieldData('JackPine', s, age) * 0.9;
YieldData('WhiteBirch', s, age) = YieldData('JackPine', s, age) * 0.8;

* =========================================================================
* 3. REGIONAL PARAMETERS
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
    initialAge(r)        /TroutLake 30, Abitibi 30, Algoma 30, Kenora 30, Nipissing 30, Algonquin 30/
    initialStockRegion(r) "Initial ROUNDWOOD stock (MILLION m3)"
                         /TroutLake 28999.48, Abitibi 107775.22, Algoma 54884.89, Kenora 49746.19, Nipissing 45261.69, Algonquin 36725.26/;

SET regionSite(r, s) "Dominant site class"
/
    TroutLake.SC2
    Abitibi.SC2
    Algoma.SC1
    Kenora.SC3
    Nipissing.SC1
    Algonquin.SC1
/;


PARAMETERS
    regionArea(r)   "Calculated hectares"
    CAI_Region(r,y) "Annual roundwood growth (MILLION m3/year)"
    initStockRW(r)  "Initial roundwood stock (MILLION m3)"
    growthRW(r,y)   "Roundwood growth (MILLION m3/year)";

regionArea(r) =
    (initialStockRegion(r) * 1000000)
    /
    sum((tree, age, s)$(ord(age)-1 = initialAge(r) and regionSite(r, s)),
        speciesMix(r, tree) * YieldData(tree, s, age));

LOOP(y,
   LOOP(r,
      CAI_Region(r, y) =
         sum(tree,
             speciesMix(r, tree) *
             (
               sum((age, s)$(ord(age)-1 = initialAge(r) + 10 and regionSite(r, s)), YieldData(tree, s, age))
             - sum((age, s)$(ord(age)-1 = initialAge(r)      and regionSite(r, s)), YieldData(tree, s, age))
             )
         ) / 10 * regionArea(r) / 1000000;
   );
);

initStockRW(r) = initialStockRegion(r);
growthRW(r,y)  = CAI_Region(r,y);


* =========================================================================
* 4. ECONOMIC / VALUE-CHAIN PARAMETERS
* =========================================================================
SCALARS
    carbonPrice   "Carbon price ($/tCO2)" /170/
    demandGrowth  "Annual demand growth rate" /0.00/;

PARAMETERS
    manufacturingTier(p) "1=Primary, 2=Secondary"
                         /CLT 2, Glulam 2, Lumber 1, Pulpwood 1/

    maxHarvestRate(r)    "Max sustainable harvest ratio"
                         /Abitibi 0.02, Algoma 0.02, Algonquin 0.02, Kenora 0.02, Nipissing 0.02, TroutLake 0.02/

    basePrice(p)         "Base price ($/m3)"
                         /CLT 700, Glulam 575, Lumber 375, Pulpwood 75/

    yieldFactor(p)       "Output per unit of primary raw-material input"
                         /CLT 0.82, Glulam 0.85, Lumber 0.90, Pulpwood 0.98/

    lumberInputPerUnit(p) "m3 lumber required per m3 product"
                         /CLT 1.20, Glulam 1.10, Lumber 0, Pulpwood 0/

    baseProcessingCost(m) "Base processing cost by layer ($/m3 output)"
                          /Primary 100, Secondary 50/

    productMarkup(p)      "Additional product-specific processing cost ($/m3 output)"
                          /CLT 150, Glulam 120, Lumber 20, Pulpwood -80/

    processingCost_y(p, y) "Processing cost over time ($/m3 output)"
    basePrice_y(p, y)      "Exogenous price path ($/m3)"
    demandCap_y(r,p,y)     "Max final demand served (MILLION m3/year)"
    baseDemand(r,p)        "Base final demand cap (MILLION m3/year)";

* Processing cost: NO fake embedded lumber cost here
processingCost_y(p, y) =
    (
      (baseProcessingCost('Primary')   + productMarkup(p))$(manufacturingTier(p)=1)
    + (baseProcessingCost('Secondary') + productMarkup(p))$(manufacturingTier(p)=2)
    ) * power(0.98, ord(y)-1);

* Exogenous product prices grow 3% annually
basePrice_y(p, y) = basePrice(p) * power(1.03, ord(y)-1);

baseDemand(r,p) = 0;

* Lumber demand
baseDemand('Nipissing','Lumber') = 6.5;  baseDemand('Abitibi','Lumber') = 5.0;
baseDemand('Algoma','Lumber')    = 4.5;  baseDemand('Algonquin','Lumber') = 4.0;
baseDemand('TroutLake','Lumber') = 3.5;  baseDemand('Kenora','Lumber') = 2.5;

* Pulpwood demand
baseDemand('Nipissing','Pulpwood') = 7.0; baseDemand('Abitibi','Pulpwood') = 6.0;
baseDemand('Algoma','Pulpwood')    = 5.0; baseDemand('Algonquin','Pulpwood') = 4.0;
baseDemand('TroutLake','Pulpwood') = 3.0; baseDemand('Kenora','Pulpwood') = 2.0;

* CLT / Glulam demand
baseDemand(r,'CLT')    = 0.17;
baseDemand(r,'Glulam') = 0.17;

demandCap_y(r,p,y) = baseDemand(r,p) * power(1 + demandGrowth, ord(y)-1);

* =========================================================================
* 5. TRANSPORT / LOGISTICS
* =========================================================================
TABLE transportBase(r,rr) "Transport cost proxy ($/m3)"
                TroutLake Abitibi Algoma Kenora Nipissing Algonquin
    TroutLake       0        38      42     100      14         9
    Abitibi        26         0      35      75      25        32
    Algoma         31        28       0      60      31        38
    Kenora         79        61      49       0      79        87
    Nipissing      14        27      31      79       0         8
    Algonquin       9        34      38      87       8         0;

PARAMETERS
    transportCost(r,rr,p)          "Transport cost by product ($/m3)"
    arcCap(r,rr,p,y)               "Arc capacity (MILLION m3/year) (shipments cannot exceed transport capacity)"
    millCap(r,p,y)                 "Mill output capacity (MILLION m3/year)"
    harvestCostRW                  "Harvest cost for roundwood ($/m3)"
    carbonEmissionFactor(p)        "Process emission factor (tCO2/m3 output)"
    carbonStorageFactor(p)         "Stored carbon factor (tCO2/m3 output)"
    transportEmissionFactor(r,rr,p) "Freight emission factor (tCO2/m3 shipped)";

transportCost(r,rr,p)$(not sameas(r,rr)) = transportBase(r,rr);

* Placeholder capacities: will be replace with real data when available
arcCap(r,rr,p,y)$(not sameas(r,rr)) = 20;

millCap(r,'Lumber',y)   = 10;
millCap(r,'Pulpwood',y) = 10;
millCap(r,'CLT',y)      = 1.0;
millCap(r,'Glulam',y)   = 1.0;

harvestCostRW = 38;

carbonEmissionFactor('CLT')      = 0.15;
carbonEmissionFactor('Glulam')   = 0.17;
carbonEmissionFactor('Lumber')   = 0.065;
carbonEmissionFactor('Pulpwood') = 0.01;

carbonStorageFactor('CLT')      = 0.9;
carbonStorageFactor('Glulam')   = 0.9;
carbonStorageFactor('Lumber')   = 0.6;
carbonStorageFactor('Pulpwood') = 0.1;

transportEmissionFactor(r,rr,p)$(not sameas(r,rr)) = 0.018;

* =========================================================================
* 6. VARIABLES
* =========================================================================
POSITIVE VARIABLES
    stockRW(r,y)          "Roundwood stock (MILLION m3)"
    harvestRW(r,y)        "Roundwood harvest (MILLION m3)"
    production(r,p,y)     "Output production (MILLION m3)"
    sales(r,p,y)          "Final sales / served demand (MILLION m3)"
    exports(r,rr,p,y)     "Interregional shipments (MILLION m3)"
    price(r,p,y)          "Price variable fixed to exogenous path ($/m3)"
    carbonStorage(r,p,y)  "Stored carbon (MtCO2 eq if factors are tCO2/m3 and quantities in million m3)"
    carbonEmission(r,p,y) "Emissions (MtCO2 eq)";

VARIABLES
    Zprofit        "Net economic value before carbon cost (MILLION $)"
    Zemission      "Total carbon emissions (MtCO2 eq)"
    carbonCost     "Carbon cost (MILLION $)"
    obj            "Objective = Zprofit - carbonCost (MILLION $)"
    revenue        "Revenue (MILLION $)"
    harvestCostTot "Harvest cost (MILLION $)"
    procCostTot    "Processing cost (MILLION $)"
    transCostTot   "Transport cost (MILLION $)";

* =========================================================================
* 7. EQUATIONS
* =========================================================================
EQUATIONS
    stockBalanceRW(r,y)
    sustainabilityRW(r,y)
    roundwoodUseEq(r,y)
    millCapacityEq(r,p,y)
    demandCapEq(r,p,y)
    arcCapEq(r,rr,p,y)
    lumberBalance(r,y)
    pulpwoodBalance(r,y)
    cltBalance(r,y)
    glulamBalance(r,y)
    carbonStoreEq(r,p,y)
    carbonEmitEq(r,p,y)
    emissionEq
    eCarbonCost
    revenueEq
    harvestCostEq
    procCostEq
    transCostEq
    profitEq
    scalarObjective;

stockBalanceRW(r,y)..
    stockRW(r,y) =E=
          (initStockRW(r) - harvestRW(r,y))$(ord(y)=1)
        + (stockRW(r,y-1) + growthRW(r,y) - harvestRW(r,y))$(ord(y)>1);

sustainabilityRW(r,y)..
    harvestRW(r,y) =L=
          maxHarvestRate(r) * initStockRW(r)$(ord(y)=1)
        + maxHarvestRate(r) * (stockRW(r,y-1) + growthRW(r,y))$(ord(y)>1);

* Roundwood can only feed primary processing (Lumber + Pulpwood)
roundwoodUseEq(r,y)..
      production(r,'Lumber',y)   / yieldFactor('Lumber')
    + production(r,'Pulpwood',y) / yieldFactor('Pulpwood')
    =L= harvestRW(r,y);

millCapacityEq(r,p,y)..
    production(r,p,y) =L= millCap(r,p,y);

demandCapEq(r,p,y)..
    sales(r,p,y) =L= demandCap_y(r,p,y);

arcCapEq(r,rr,p,y)$(not sameas(r,rr))..
    exports(r,rr,p,y) =L= arcCap(r,rr,p,y);

* Lumber balance: final lumber sales + lumber shipments + lumber used in CLT/Glulam
lumberBalance(r,y)..
      production(r,'Lumber',y)
    + sum(rr$(not sameas(r,rr)), exports(rr,r,'Lumber',y))
    =E=
      sales(r,'Lumber',y)
    + sum(rr$(not sameas(r,rr)), exports(r,rr,'Lumber',y))
    + lumberInputPerUnit('CLT')    * production(r,'CLT',y)
    + lumberInputPerUnit('Glulam') * production(r,'Glulam',y);

pulpwoodBalance(r,y)..
      production(r,'Pulpwood',y)
    + sum(rr$(not sameas(r,rr)), exports(rr,r,'Pulpwood',y))
    =E=
      sales(r,'Pulpwood',y)
    + sum(rr$(not sameas(r,rr)), exports(r,rr,'Pulpwood',y));

cltBalance(r,y)..
      production(r,'CLT',y)
    + sum(rr$(not sameas(r,rr)), exports(rr,r,'CLT',y))
    =E=
      sales(r,'CLT',y)
    + sum(rr$(not sameas(r,rr)), exports(r,rr,'CLT',y));

glulamBalance(r,y)..
      production(r,'Glulam',y)
    + sum(rr$(not sameas(r,rr)), exports(rr,r,'Glulam',y))
    =E=
      sales(r,'Glulam',y)
    + sum(rr$(not sameas(r,rr)), exports(r,rr,'Glulam',y));

carbonStoreEq(r,p,y)..
    carbonStorage(r,p,y) =E= carbonStorageFactor(p) * production(r,p,y);

carbonEmitEq(r,p,y)..
    carbonEmission(r,p,y) =E=
        carbonEmissionFactor(p) * production(r,p,y)
      + sum(rr$(not sameas(r,rr)), transportEmissionFactor(r,rr,p) * exports(r,rr,p,y));

emissionEq..
    Zemission =E= sum((r,p,y), carbonEmission(r,p,y));

eCarbonCost..
    carbonCost =E= carbonPrice * Zemission;

revenueEq..
    revenue =E= sum((r,p,y), price(r,p,y) * sales(r,p,y));

harvestCostEq..
    harvestCostTot =E= sum((r,y), harvestCostRW * harvestRW(r,y));

procCostEq..
    procCostTot =E= sum((r,p,y), processingCost_y(p,y) * production(r,p,y));

transCostEq..
    transCostTot =E= sum((r,rr,p,y)$(not sameas(r,rr)), transportCost(r,rr,p) * exports(r,rr,p,y));

profitEq..
    Zprofit =E= revenue - harvestCostTot - procCostTot - transCostTot;

scalarObjective..
    obj =E= Zprofit - carbonCost;

* =========================================================================
* 8. BOUNDS / FIXED PRICES / INITIAL LEVELS
* =========================================================================
* Fix prices to the exogenous 3% growth path
price.fx(r,p,y) = basePrice_y(p,y);

* No within-region shipments
exports.fx(r,r,p,y) = 0;

* Keep some stock floor in 2025 if you want a policy constraint
stockRW.lo(r,'2025') = 0.5 * initStockRW(r);

* Starting levels
stockRW.l(r,y)      = initStockRW(r);
harvestRW.l(r,y)    = maxHarvestRate(r) * initStockRW(r);
production.l(r,p,y) = 0.25 * demandCap_y(r,p,y);
sales.l(r,p,y)      = 0.25 * demandCap_y(r,p,y);
exports.l(r,rr,p,y)$(not sameas(r,rr)) = 0;

* =========================================================================
* 9. MODEL AND SOLVE
* =========================================================================
MODEL ForestCP /
    stockBalanceRW, sustainabilityRW, roundwoodUseEq,
    millCapacityEq, demandCapEq, arcCapEq,
    lumberBalance, pulpwoodBalance, cltBalance, glulamBalance,
    carbonStoreEq, carbonEmitEq,
    emissionEq, eCarbonCost,
    revenueEq, harvestCostEq, procCostEq, transCostEq,
    profitEq, scalarObjective
/;

SOLVE ForestCP USING NLP MAXIMIZING obj;

* =========================================================================
* 10. REPORTING
* =========================================================================
PARAMETER
    tierMetrics(r,m,met,y)      "Manufacturing layer metrics"
    productChainCost(p,comp,y)  "Product cost breakdown"
    revRPY(r,p,y)               "Revenue by region/product/year (MILLION $)"
    revR(r)                     "Total revenue by region (MILLION $)"
    rev2025(r)                  "Revenue by region in 2025 (MILLION $)";

tierMetrics(r,m,'Production',y) =
    sum(p$(manufacturingTier(p)=ord(m)), production.l(r,p,y));

tierMetrics(r,m,'Revenue',y) =
    sum(p$(manufacturingTier(p)=ord(m)), price.l(r,p,y) * sales.l(r,p,y));

tierMetrics(r,m,'ProcessingCost',y) =
    sum(p$(manufacturingTier(p)=ord(m)), processingCost_y(p,y) * production.l(r,p,y));

productChainCost(p,'BaseProcessing',y) =
      baseProcessingCost('Primary')$(manufacturingTier(p)=1)
    + baseProcessingCost('Secondary')$(manufacturingTier(p)=2);

productChainCost(p,'ProductMarkup',y) = productMarkup(p);
productChainCost(p,'LumberInputCost',y) = basePrice_y('Lumber',y) * lumberInputPerUnit(p);
productChainCost(p,'Total',y) = processingCost_y(p,y);

revRPY(r,p,y) = price.l(r,p,y) * sales.l(r,p,y);
revR(r)       = sum((p,y), revRPY(r,p,y));
rev2025(r)    = sum(p, revRPY(r,p,'2025'));

DISPLAY
    Zprofit.l, Zemission.l, carbonCost.l, obj.l,
    tierMetrics, productChainCost,
    price.l, stockRW.l, harvestRW.l, production.l, exports.l, sales.l,
    revenue.l, harvestCostTot.l, procCostTot.l, transCostTot.l,
    revR, rev2025, CAI_Region, regionArea, speciesMix;

* =========================================================================
* 11. EXPORTS
* =========================================================================
Execute_Unload "ForestResults.gdx",
    obj, Zprofit, Zemission, carbonCost,
    price, stockRW, harvestRW, production, exports, sales,
    revenue, harvestCostTot, procCostTot, transCostTot,
    revR, rev2025, tierMetrics;

Execute "gdx2csv ForestResults.gdx";

Execute_Unload "output/ForestResults.gdx",
    obj, Zprofit, Zemission, carbonCost,
    price, stockRW, harvestRW, production, exports, sales,
    revenue, harvestCostTot, procCostTot, transCostTot,
    revR, rev2025, tierMetrics;

Execute "gdx2csv output/ForestResults.gdx output=output/results_summary.csv";

$log Corrected roundwood-based model finished. Prices were fixed to basePrice_y to prevent artificial endogenous drift.