const fs = require('fs');

const ptf = JSON.parse(fs.readFileSync('presentation/ptf_coefs_cons.json', 'utf8'));
const coef_data = JSON.parse(fs.readFileSync('presentation/calculator_coefs.json', 'utf8'));

let sd = ptf.scales;
let c = ptf.coefficients;

let soil_pH = 6.0;
let soil_clay_silt = 40;

let z_pH = (soil_pH - sd.pH.mean) / sd.pH.sd;
let z_tex = (Math.log(soil_clay_silt) - sd.FineTexture.mean) / sd.FineTexture.sd;

let val_Ca = 5000;
let val_Mg = 250;
let val_K = 115;
let val_Corg = 2.0;

let z_Ca = (Math.log(val_Ca) - sd.Ca.mean) / sd.Ca.sd;
let z_Mg = (Math.log(val_Mg) - sd.Mg.mean) / sd.Mg.sd;
let z_K = (Math.log(val_K) - sd.K.mean) / sd.K.sd;
let z_Corg = (Math.log(val_Corg) - sd.Corg.mean) / sd.Corg.sd;

let ln_K = c["(Intercept)"] + c["z_ln_FineTexture"]*z_tex + c["z_pH"]*z_pH + 
           c["z_ln_Ca"]*z_Ca + c["z_ln_Mg"]*z_Mg + c["z_ln_K"]*z_K + c["z_ln_Corg"]*z_Corg;

let n = c["ln_a_CO2"] + c["ln_a_CO2:z_ln_FineTexture"]*z_tex + c["ln_a_CO2:z_pH"]*z_pH + 
        c["ln_a_CO2:z_ln_Ca"]*z_Ca + c["ln_a_CO2:z_ln_Mg"]*z_Mg + c["ln_a_CO2:z_ln_K"]*z_K + 
        c["ln_a_CO2:z_ln_Corg"]*z_Corg;

// Wait, the OJS code uses c["ln_P_CO2"] but the json has "ln_a_CO2" !!!
// Let's check!
console.log("n from ln_P_CO2:", c["ln_P_CO2"]);
console.log("n from ln_a_CO2:", c["ln_a_CO2"]);

