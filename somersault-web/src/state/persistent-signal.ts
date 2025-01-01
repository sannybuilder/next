import { signal } from '@angular/core';

export function persistentSignal<T>(name: string, initialValue: T) {
  const item = localStorage.getItem(name);
  const value = (item && JSON.parse(item)) || initialValue;
  const s = signal(value);
  const originalSet = s.set;
  s.set = (value) => {
    originalSet(value);
    localStorage.setItem(name, JSON.stringify(value));
  };

  return s;
}
