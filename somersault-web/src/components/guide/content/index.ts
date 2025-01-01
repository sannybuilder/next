import welcome from './welcome';
import basicTypes from './base-types';
import functions from './functions';
import variables from './variables';
import controlFlow from './control-flow';
import loops from './loops';
import arrays from './arrays';
import pointers from './pointers';
import qsort from './qsort';
import expressions from './expressions';
import logicalExpressions from './logical-expressions';
import constants from './constants';
import modules from './modules';

export default [
  {
    title: 'Welcome',
    code: welcome,
  },
  {
    title: 'Basic Types',
    code: basicTypes,
  },
  {
    title: 'Variables',
    code: variables,
  },
  {
    title: 'Arrays',
    code: arrays,
  },
  {
    title: 'Functions',
    code: functions,
  },
  {
    title: 'Control Flow',
    code: controlFlow,
  },
  {
    title: 'Expressions',
    code: expressions,
  },
  {
    title: 'Logical Expressions',
    code: logicalExpressions,
  },
  {
    title: 'Constants',
    code: constants,
  },
  {
    title: 'Loops',
    code: loops,
  },
  {
    title: 'Pointers',
    code: pointers,
  },
  // {
  //   title: 'CLEO 5 Modules',
  //   code: modules,
  // },
  {
    title: 'Example: Quicksort',
    code: qsort,
  },
].map((page) => ({
  ...page,
  slug: slugify(page.title),
}));

function slugify(title: string) {
  return title
    .toLowerCase()
    .replace(/[^a-z0-9 -]/g, '')
    .replace(/\s+/g, '-')
    .replace(/-+/g, '-');
}
