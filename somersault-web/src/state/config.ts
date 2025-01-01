import { InjectionToken } from '@angular/core';

const CONFIG = new InjectionToken('config');

export default CONFIG;

export interface Config {
  production: boolean;
}
